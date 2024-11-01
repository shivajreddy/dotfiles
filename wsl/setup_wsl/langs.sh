#!/bin/bash

#############################################
# Languages and Package Managers Script
# Author: Shiva
# Version: 1.0
# Check "NOTE" comments before running
#############################################

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
# Go Lang Installation Functions
#############################################

# Variables
# NOTE: Make sure this is the latest go lang version, or else nvim lsp will 🥹
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

# Main function to install Go
install_go() {
    log "info" "Starting Go installation..."
    remove_existing_go
    download_go
    extract_go
    update_path
    apply_path
    verify_go
    cleanup
    log "done" "Go installation process completed."
}

#############################################
# Python Installation Functions
#############################################

# Function to install Python
install_python() {
    log "info" "Starting Python installation..."

    # Check if Python 3.12 is already installed
    if command -v python3.12 >/dev/null 2>&1; then
        log "warn" "Python3.12 is already installed. Skipping installation."
        return
    fi

    # Update package list
    log "info" "Updating package list..."
    sudo apt update
    if [ $? -ne 0 ]; then
        log "error" "Failed to update package list."
        exit 1
    fi

    # Install build dependencies
    log "info" "Installing build dependencies..."
    sudo apt install -y build-essential libreadline-dev libncursesw5-dev libssl-dev \
        libsqlite3-dev libgdbm-dev libbz2-dev liblzma-dev zlib1g-dev uuid-dev \
        libffi-dev libdb-dev wget curl
    if [ $? -ne 0 ]; then
        log "error" "Failed to install build dependencies."
        exit 1
    fi

    # Create a temporary directory for building Python
    TEMP_DIR=$(mktemp -d)
    if [ ! -d "$TEMP_DIR" ]; then
        log "error" "Failed to create a temporary directory."
        exit 1
    fi
    log "info" "Created temporary directory at $TEMP_DIR."

    # Function to clean up temporary files
    cleanup() {
        if [ -d "$TEMP_DIR" ]; then
            rm -rf "$TEMP_DIR"
            if [ $? -eq 0 ]; then
                log "info" "Cleaned up temporary files."
            else
                log "warn" "Failed to clean up temporary files at $TEMP_DIR. Please remove them manually."
            fi
        fi
    }

    # Ensure that cleanup is called on script exit, whether successful or due to an error
    trap cleanup EXIT

    # Navigate to the temporary directory
    cd "$TEMP_DIR" || {
        log "error" "Failed to enter temporary directory."
        exit 1
    }

    # Download Python 3.12 source tarball
    log "info" "Downloading Python 3.12 source..."
    wget https://www.python.org/ftp/python/3.12.0/Python-3.12.0.tgz
    if [ $? -ne 0 ]; then
        log "error" "Failed to download Python 3.12."
        exit 1
    fi

    # Extract the tarball
    log "info" "Extracting Python 3.12 source..."
    tar xzf Python-3.12.0.tgz
    if [ $? -ne 0 ]; then
        log "error" "Failed to extract Python 3.12 source."
        exit 1
    fi

    # Navigate into the extracted directory
    cd Python-3.12.0 || {
        log "error" "Failed to enter Python source directory."
        exit 1
    }

    # Configure the build with optimizations
    log "info" "Configuring the Python build with optimizations..."
    ./configure --enable-optimizations
    if [ $? -ne 0 ]; then
        log "error" "Configuration failed."
        exit 1
    fi

    # Compile the source code using all available CPU cores
    log "info" "Compiling Python 3.12 (this may take a while)..."
    make -j "$(nproc)"
    if [ $? -ne 0 ]; then
        log "error" "Compilation failed."
        exit 1
    fi

    # Install Python using altinstall to prevent overwriting the default python3 binary
    log "info" "Installing Python 3.12..."
    sudo make altinstall
    if [ $? -ne 0 ]; then
        log "error" "Installation failed."
        exit 1
    fi

    # Verify the Python installation
    log "info" "Verifying Python 3.12 installation..."
    if python3.12 --version >/dev/null 2>&1; then
        log "info" "Python 3.12 installed successfully! Version: $(python3.12 --version)"
    else
        log "error" "Python 3.12 installation failed."
        exit 1
    fi

    # Install pip for Python 3.12
    log "info" "Installing pip for Python 3.12..."
    curl -sS https://bootstrap.pypa.io/get-pip.py -o get-pip.py
    if [ $? -ne 0 ]; then
        log "error" "Failed to download get-pip.py."
        exit 1
    fi
    sudo python3.12 get-pip.py
    if [ $? -ne 0 ]; then
        log "error" "Failed to install pip for Python 3.12."
        exit 1
    fi
    log "info" "pip installed successfully for Python 3.12."

    # Optional: Create a symbolic link for pip3.12 (if desired)
    # sudo ln -s /usr/local/bin/pip3.12 /usr/local/bin/pip3.12

    # Skip upgrading system pip to avoid PEP 668 issues
    log "warn" "Skipping system pip upgrade to avoid conflicts."

    log "done" "Python installation process completed successfully."
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
# Main Installation Function
#############################################

# Function to install all components
install_all() {
    # install_go
    install_python
    # install_nvm
    # install_node
    # verify_npm
    log "done" "All installations completed successfully."
}

#############################################
# Execute Installation
#############################################

install_all
