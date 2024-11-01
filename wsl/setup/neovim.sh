#!/bin/bash

# Neovim and LazyVim Installation Script
# Author: Your Name
# Version: 1.0

# Color codes for better output
GREEN='\033[0;32m'
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
        *)
            prefix="[*]"
            ;;
    esac

    echo -e "${color}${prefix} ${message}${NC}"
}

# Validate system requirements
validate_system() {
    log "info" "Checking system requirements..."

    # Check for required tools
    local required_tools=("curl" "wget" "git")
    for tool in "${required_tools[@]}"; do
        if ! command -v "$tool" &> /dev/null; then
            log "error" "$tool is not installed. Please install it first."
            exit 1
        fi
    done
}

# Install dependencies
install_dependencies() {
    log "info" "Installing Neovim dependencies..."
    
    local dependencies=("fuse" "libfuse2" "unzip" "gzip")
    for dep in "${dependencies[@]}"; do
        if ! dpkg -s "$dep" &> /dev/null; then
            sudo apt-get install -y "$dep" || {
                log "error" "Failed to install $dep"
                exit 1
            }
            log "info" "$dep installed successfully"
        else
            log "info" "$dep is already installed"
        fi
    done
}

# Install Neovim
install_neovim() {
    log "info" "Downloading and installing Neovim..."

    # Fetch the latest release information
    local release_info=$(curl -s https://api.github.com/repos/neovim/neovim/releases/latest)

    # Extract the download URL for nvim.appimage
    local download_url=$(echo "$release_info" | grep -o 'https://[^"]*nvim.appimage"' | tr -d '"')

    if [ -z "$download_url" ]; then
        log "error" "Could not fetch Neovim download URL"
        exit 1
    fi

    # Download AppImage
    log "info" "Downloading Neovim AppImage from: $download_url"
    curl -L -o nvim.appimage "$download_url"

    # Make executable
    chmod +x nvim.appimage

    # Try extraction
    ./nvim.appimage --appimage-extract > /dev/null 2>&1 || {
        log "error" "Failed to extract AppImage"
        exit 1
    }

    # Move extracted files
    sudo mv squashfs-root /opt/nvim

    # Create symlink
    sudo ln -sf /opt/nvim/AppRun /usr/local/bin/nvim

    # Clean up
    rm nvim.appimage

    # Verify installation
    if command -v nvim &> /dev/null; then
        log "info" "Neovim $(nvim --version | head -n 1) installed successfully!"
    else
        log "error" "Neovim installation failed"
        exit 1
    fi
}


# Clean up old Neovim configurations
cleanup_old_config() {
    log "info" "Cleaning up old Neovim configurations..."
    
    local dirs=(
        ~/.local/share/nvim
        ~/.local/state/nvim
        ~/.cache/nvim
        ~/.config/nvim
    )

    for dir in "${dirs[@]}"; do
        if [ -d "$dir" ]; then
            rm -rf "$dir"
            log "warn" "Deleted $dir"
        fi
    done
}

# Setup LazyVim
setup_lazyvim() {
    log "info" "Setting up LazyVim starter template..."
    
    # Clone LazyVim starter template
    git clone https://github.com/LazyVim/starter ~/.config/nvim
    
    # Remove .git directory to make it your own
    rm -rf ~/.config/nvim/.git
    
    log "info" "LazyVim starter template installed. First run will install plugins."
}

# Make sym links to my nvim dotfiles
# Make sym links to my nvim dotfiles
create_symlink() {
    log "info" "Creating symlinks for Neovim configuration..."

    # Define the nvim path
    local NVIM_CONFIG_PATH="$HOME/.config/nvim"
    local DOTFILES_NVIM_PATH="$HOME/dotfiles/wsl/nvim"

    # Check if dotfiles nvim path exists
    if [ ! -d "$DOTFILES_NVIM_PATH" ]; then
        log "error" "Dotfiles Neovim path not found: $DOTFILES_NVIM_PATH"
        exit 1
    fi

    # Check if current nvim config is a symlink
    if [ -L "$NVIM_CONFIG_PATH" ]; then
        log "warn" "Existing symlink found. Removing current symlink."
        unlink "$NVIM_CONFIG_PATH"
    # Check if current nvim config is a directory
    elif [ -d "$NVIM_CONFIG_PATH" ]; then
        log "warn" "Existing Neovim config found. Deleting ${NVIM_CONFIG_PATH}"
        rm -rf "$NVIM_CONFIG_PATH"
    fi

    # Create symlink
    ln -sf "$DOTFILES_NVIM_PATH" "$NVIM_CONFIG_PATH"

    # Verify symlink
    if [ -L "$NVIM_CONFIG_PATH" ]; then
        log "info" "Symlink created successfully: $NVIM_CONFIG_PATH -> $DOTFILES_NVIM_PATH"
    else
        log "error" "Failed to create symlink"
        exit 1
    fi
}

# Main execution
main() {
    clear
    echo -e "${GREEN}🔥 Neovim and LazyVim Installation Script${NC}"
    
    validate_system
    install_dependencies
    cleanup_old_config
    install_neovim
    setup_lazyvim
    create_symlink
    
    log "info" "Installation complete! Run 'nvim' to start setup."
}

# Run the script
main
