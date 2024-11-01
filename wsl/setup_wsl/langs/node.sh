#!/bin/bash

#############################################
# Script for installing: Node, NPM, NVM
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
# NVM Installation Functions
#############################################

# Function to install NVM
install_nvm() {
    log "info" "Starting NVM (Node Version Manager) installation..."

    # Check if NVM is already installed
    if [ -d "$HOME/.nvm" ]; then
        log "warn" "NVM is already installed. Skipping installation."
        return
    fi

    # Download and install NVM
    log "info" "Downloading and installing NVM..."
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.5/install.sh | bash
    if [ $? -ne 0 ]; then
        log "error" "Failed to install NVM."
        exit 1
    fi
    log "pass" "NVM installation script executed successfully."

    # Load NVM into current shell
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    if [ $? -ne 0 ]; then
        log "error" "Failed to load NVM."
        exit 1
    fi

    # Verify NVM installation
    if command -v nvm >/dev/null 2>&1; then
        log "info" "NVM installed successfully! Version: $(nvm --version)"
    else
        log "error" "NVM installation failed."
        exit 1
    fi

    log "done" "NVM installation process completed."
}

#############################################
# Node.js Installation Functions
#############################################

# Function to install Node.js using NVM
install_node() {
    log "info" "Starting Node.js installation..."

    # Check if NVM is installed
    if ! command -v nvm >/dev/null 2>&1; then
        log "error" "NVM is not installed. Please install NVM first."
        exit 1
    fi

    # Install the latest LTS version of Node.js
    log "info" "Installing the latest LTS version of Node.js..."
    nvm install --lts
    if [ $? -ne 0 ]; then
        log "error" "Failed to install Node.js."
        exit 1
    fi
    log "info" "Node.js installed successfully."

    # Set the installed Node.js version as default
    nvm use --lts
    nvm alias default 'lts/*'

    log "done" "Node.js installation process completed."
}

#############################################
# NPM Installation Functions
#############################################

# Function to verify NPM installation
verify_npm() {
    log "info" "Verifying NPM installation..."
    if command -v npm >/dev/null 2>&1; then
        log "info" "NPM is installed successfully! Version: $(npm -v)"
    else
        log "error" "NPM installation failed."
        exit 1
    fi
}

#############################################
# Execute Installation
#############################################

main() {
    ensure_sudo

    log "info" "Starting nvm installation..."
    install_nvm

    log "info" "Starting node installation..."
    install_node

    log "info" "Starting npm installation..."
    verify_npm

    log "done" "node, nvm, npm installations completed successfully."
}

main
