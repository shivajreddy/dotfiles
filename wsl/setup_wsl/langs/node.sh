#!/bin/bash

#############################################
# Script for installing: Node, NPM, NVM
# Author: Shiva
# Version: 1.1
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
# Run this script as a non-root user
#############################################

# Function to ensure script does NOT run with sudo privileges
ensure_non_sudo() {
    if [ "$EUID" -eq 0 ]; then
        log "error" "Please do NOT run this script with sudo or as root."
        exit 1
    fi
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

    # Detect the user's default shell
    USER_SHELL=$(basename "$SHELL")
    case $USER_SHELL in
    "bash")
        SHELL_RC="$HOME/.bashrc"
        ;;
    "zsh")
        SHELL_RC="$HOME/.zshrc"
        ;;
    *)
        log "warn" "Unsupported shell: $USER_SHELL. Please source NVM manually."
        return
        ;;
    esac

    # Load NVM into current shell by appending to shell rc file
    log "info" "Configuring NVM in $SHELL_RC..."
    {
        echo ""
        echo "# NVM Configuration"
        echo 'export NVM_DIR="$HOME/.nvm"'
        echo '[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm'
        echo '[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion'
    } >>"$SHELL_RC"

    log "pass" "NVM configuration added to $SHELL_RC."

    # Source the shell rc file to make NVM available in the current session
    log "info" "Loading NVM into the current shell..."
    source "$SHELL_RC"

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
# NPM Verification Functions
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
    ensure_non_sudo

    log "info" "Starting NVM installation..."
    install_nvm

    log "info" "Starting Node.js installation..."
    install_node

    log "info" "Starting NPM verification..."
    verify_npm

    log "done" "Node.js, NVM, and NPM installations completed successfully."
    log "warn" "Please restart your terminal or run 'source ~/.bashrc' or 'source ~/.zshrc' to apply changes."
}

main
