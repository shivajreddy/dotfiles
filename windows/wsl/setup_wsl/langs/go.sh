#!/bin/bash

#############################################
# Go Lang Script
# Author: Shiva
# Version: 1.0
# Check "NOTE" comments before running
#############################################

# Exit on any error
set -e

# Color codes for better output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
PINK='\033[0;35m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Logging function
log() {
    local type=$1
    local message=$2
    local color=$NC

    case $type in
    "info")
        color=$GREEN
        prefix="[ℹ]"
        ;;
    "warn")
        color=$YELLOW
        prefix="[⚠]"
        ;;
    "error")
        color=$RED
        prefix="[✖]"
        ;;
    "pass")
        color=$PINK
        prefix="[✅]"
        ;;
    "done")
        color=$BLUE
        prefix="[🌟]"
        ;;
    *)
        prefix="[*]"
        ;;
    esac

    echo -e "${color}${prefix} ${message}${NC}"
}

#############################################
# Run this script as root user
#############################################

# Function to ensure script runs with sudo privileges
ensure_sudo() {
    if [ "$EUID" -ne 0 ]; then
        log "warn" "This script needs sudo privileges to run"
        if ! sudo -v; then
            log "error" "Failed to obtain sudo privileges"
            exit 1
        fi

        # Re-run the script with sudo
        log "info" "Restarting script with sudo..."
        exec sudo "$0" "$@"
    fi
    log "pass" "Running with root privileges"
}

#############################################
# Go Lang Installation Functions
#############################################

# Variables
# NOTE: Make sure to use latest version, or else nvim's lsp will cry
GO_VERSION="go1.23.2.linux-amd64.tar.gz"
GO_URL="https://dl.google.com/go/$GO_VERSION"
PROFILE_FILE="$HOME/.profile"

# Function to remove existing Go installation
remove_existing_go() {
    log "info" "Removing any existing Go installation from /usr/local/go..."
    if [ -d "/usr/local/go" ]; then
        sudo rm -rf /usr/local/go
        if [ $? -eq 0 ]; then
            log "info" "Previous Go installation removed."
        else
            log "error" "Failed to remove existing Go installation."
            exit 1
        fi
    else
        log "warn" "No previous Go installation found."
    fi
}

# Function to download Go tarball
download_go() {
    log "info" "Downloading Go $GO_VERSION..."
    curl -LO "$GO_URL"
    if [ $? -ne 0 ]; then
        log "error" "Failed to download Go. Check your internet connection or URL."
        exit 1
    fi
    log "pass" "Download completed."
}

# Function to extract Go tarball
extract_go() {
    log "info" "Extracting Go to /usr/local..."
    sudo tar -C /usr/local -xzf "$GO_VERSION"
    if [ $? -ne 0 ]; then
        log "error" "Extraction failed. Ensure you have the necessary permissions."
        exit 1
    fi
    log "pass" "Go extracted successfully to /usr/local/go."
}

# Function to update PATH
update_path() {
    log "info" "Updating PATH in $PROFILE_FILE..."
    if ! grep -q "/usr/local/go/bin" "$PROFILE_FILE"; then
        echo 'export PATH=$PATH:/usr/local/go/bin' >>"$PROFILE_FILE"
        if [ $? -eq 0 ]; then
            log "info" "PATH updated in $PROFILE_FILE."
        else
            log "error" "Failed to update PATH in $PROFILE_FILE."
            exit 1
        fi
    else
        log "warn" "PATH already includes /usr/local/go/bin in $PROFILE_FILE."
    fi
}

# Function to apply updated PATH
apply_path() {
    log "info" "Applying the updated PATH..."
    source "$PROFILE_FILE"
    if [ $? -eq 0 ]; then
        log "pass" "PATH applied successfully."
    else
        log "error" "Failed to apply PATH changes."
        exit 1
    fi
}

# Function to verify Go installation
verify_go() {
    log "info" "Verifying Go installation..."
    if go version >/dev/null 2>&1; then
        log "info" "Go installation successful! Installed version: $(go version)"
    else
        log "error" "Go installation failed. Please check the steps or try again."
        exit 1
    fi
}

# Function to clean up downloaded files
cleanup() {
    log "info" "Cleaning up downloaded files..."
    rm -f "$GO_VERSION"
    if [ $? -eq 0 ]; then
        log "info" "Cleanup completed."
    else
        log "warn" "Failed to remove $GO_VERSION. Please remove it manually."
    fi
}

#############################################
# Main Installation Function
#############################################

# Main function to install Go
install_go() {
    ensure_sudo

    log "info" "Starting Go installation..."
    remove_existing_go
    download_go
    extract_go

    # update path variables
    update_path
    apply_path

    verify_go
    cleanup
    log "done" "Go installation process completed."
}

#############################################
# Execute Installation
#############################################

install_go
