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

# Function to detect OS
detect_os() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        OS=$ID
        VERSION_ID=$VERSION_ID
        VERSION_CODENAME=$VERSION_CODENAME
        log "info" "Detected OS: $OS $VERSION_ID ($VERSION_CODename)"
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
    apt-get update -y

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

# Installation function
install_python() {
    log "info" "Updating package lists..."
    apt-get update -y

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

    # Determine the installed Python version
    LATEST_PYTHON=$(ls /usr/bin/python3.* 2>/dev/null | grep -v "config\|m$" | sort -V | tail -n1 | xargs basename)
    if [ -z "$LATEST_PYTHON" ]; then
        log "error" "Failed to determine the installed Python version."
        exit 1
    fi

    setup_pip_and_aliases $LATEST_PYTHON

    log "done" "Installation complete!"
    log "warn" "Please log out and log back in for the aliases to take effect."
    log "info" "Installed Python version: $(python3 --version)"
}

#############################################
# Execute Installation
#############################################

main() {
    ensure_sudo

    log "info" "Starting Python installation script..."
    check_root
    detect_os

    install_python

    log "done" "All installations completed successfully."
}

main
