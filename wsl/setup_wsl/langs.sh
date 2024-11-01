#!/bin/bash

#############################################
# Languages and Package Managers Script
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

# Function to check if running as root
check_root() {
    if [ "$EUID" -ne 0 ]; then
        log "error" "Please run as root"
        exit 1
    fi
    log "pass" "Running with root privileges"
}

# Function to detect OS
detect_os() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        OS=$ID
        VERSION_ID=$VERSION_ID
        log "info" "Detected OS: $OS $VERSION_ID"
        return 0
    else
        log "error" "Cannot detect OS"
        exit 1
    fi
}

# Function to install Python on Debian
install_python_debian() {
    log "info" "Installing Python on Debian..."

    # Enable backports for latest Python
    if ! grep -q "deb http://deb.debian.org/debian ${VERSION_CODENAME}-backports main" /etc/apt/sources.list; then
        log "info" "Adding backports repository..."
        echo "deb http://deb.debian.org/debian ${VERSION_CODENAME}-backports main" >>/etc/apt/sources.list
    fi

    log "info" "Updating package lists..."
    apt-get update

    log "info" "Installing build dependencies..."
    apt-get install -y build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev libsqlite3-dev wget libbz2-dev

    # Try to install from backports first
    if apt-cache -t ${VERSION_CODENAME}-backports search "^python3\.[0-9]+$" >/dev/null 2>&1; then
        LATEST_PYTHON=$(apt-cache -t ${VERSION_CODENAME}-backports search "^python3\.[0-9]+$" | tail -n1 | cut -d' ' -f1)
        log "info" "Installing Python from backports: $LATEST_PYTHON"
        apt-get -t ${VERSION_CODENAME}-backports install -y $LATEST_PYTHON ${LATEST_PYTHON}-venv ${LATEST_PYTHON}-distutils
    else
        # If not available in backports, install the latest from main repo
        LATEST_PYTHON=$(apt-cache search "^python3\.[0-9]+$" | tail -n1 | cut -d' ' -f1)
        log "info" "Installing Python from main repository: $LATEST_PYTHON"
        apt-get install -y $LATEST_PYTHON ${LATEST_PYTHON}-venv ${LATEST_PYTHON}-distutils
    fi

    return 0
}

# Function to install Python on Ubuntu
install_python_ubuntu() {
    log "info" "Installing Python on Ubuntu..."

    log "info" "Installing prerequisites..."
    apt-get install -y software-properties-common

    log "info" "Adding deadsnakes PPA..."
    add-apt-repository -y ppa:deadsnakes/ppa

    log "info" "Updating package lists..."
    apt-get update

    LATEST_PYTHON=$(apt-cache search "^python3\.[0-9]+$" | tail -n1 | cut -d' ' -f1)
    log "info" "Installing latest Python: $LATEST_PYTHON"
    apt-get install -y $LATEST_PYTHON ${LATEST_PYTHON}-venv ${LATEST_PYTHON}-distutils

    return 0
}

# Function to setup pip and aliases
setup_pip_and_aliases() {
    local LATEST_PYTHON=$1

    log "info" "Installing pip..."
    curl -sS https://bootstrap.pypa.io/get-pip.py | ${LATEST_PYTHON}
    log "pass" "Pip installation complete"

    # Extract version number (e.g., 3.10)
    PYTHON_VERSION=$(echo $LATEST_PYTHON | sed 's/python//')

    # Create alternatives for python and python3
    log "info" "Setting up alternatives..."
    update-alternatives --install /usr/bin/python python /usr/bin/$LATEST_PYTHON 1
    update-alternatives --install /usr/bin/python3 python3 /usr/bin/$LATEST_PYTHON 1
    log "pass" "Alternatives configured"

    # Create aliases
    log "info" "Setting up aliases..."
    cat >/etc/profile.d/python-aliases.sh <<EOF
alias py='python${PYTHON_VERSION}'
alias python='python${PYTHON_VERSION}'
EOF

    log "info" "Making aliases executable..."
    chmod +x /etc/profile.d/python-aliases.sh
    log "pass" "Aliases configured"
}

# Installation function
install_python() {
    log "info" "Updating package lists..."
    apt-get update

    case $OS in
    "debian")
        install_python_debian
        ;;
    "ubuntu")
        install_python_ubuntu
        ;;
    *)
        log "error" "Unsupported operating system: $OS"
        exit 1
        ;;
    esac

    LATEST_PYTHON=$(ls /usr/bin/python3.* | grep -v "config\|m$" | sort -V | tail -n1 | xargs basename)
    setup_pip_and_aliases $LATEST_PYTHON

    log "done" "Installation complete!"
    log "warn" "Please log out and log back in for the aliases to take effect."
    log "info" "Installed Python version: $(python3 --version)"
}

# Main Installation function
main_install_python() {
    log "info" "Starting Python installation script..."
    check_root
    detect_os
    install_python
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
    main_install_python
    # install_nvm
    # install_node
    # verify_npm
    log "done" "All installations completed successfully."
}

#############################################
# Execute Installation
#############################################

install_all
