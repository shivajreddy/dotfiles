#!/bin/bash

#############################################
# Script for: Python & Pip
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

install_python3_12() {
    log "info" "Installing Python 3.12..."

    # Create a temporary directory
    local temp_dir=$(mktemp -d)
    cd "$temp_dir" || {
        log "error" "Failed to create temporary directory"
        exit 1
    }

    # Download Python 3.12 source
    PYTHON_VERSION="3.12.0"
    curl -Lo python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tar.xz"

    # Extract and navigate to source directory
    tar -xf python.tar.xz
    cd "Python-${PYTHON_VERSION}" || {
        log "error" "Failed to enter Python source directory"
        exit 1
    }

    # Install dependencies for building Python
    sudo apt update
    sudo apt install -y build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev \
        libssl-dev libreadline-dev libffi-dev libsqlite3-dev libbz2-dev

    # Configure and compile
    ./configure --enable-optimizations
    make -j "$(nproc)"

    # Install Python
    sudo make altinstall

    # Verify installation
    if python3.12 --version &>/dev/null; then
        log "info" "Python 3.12 installed successfully!"
    else
        log "error" "Python 3.12 installation failed"
        exit 1
    fi

    # Clean up
    cd - >/dev/null
    rm -rf "$temp_dir"

    # Set up alternatives and alias for python3
    sudo update-alternatives --install /usr/bin/python3 python3 /usr/local/bin/python3.12 1
    log "info" "Python 3.12 set as the default Python 3 version."

    # Verify final version
    python3 --version
}

# Function to setup pip and aliases
setup_pip_and_aliases() {
    local LATEST_PYTHON=$1

    log "info" "Installing pip using apt..."
    apt-get install -y python3-pip
    log "pass" "Pip installation complete"

    # Extract version number (e.g., 3.11)
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

#############################################
# Execute Installation
#############################################

main() {
    ensure_sudo

    log "info" "Starting Python installation script..."
    check_root

    install_python3_12

    log "done" "All installations completed successfully."
}

main
