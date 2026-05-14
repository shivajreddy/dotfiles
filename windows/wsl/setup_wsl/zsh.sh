#!/bin/bash

# Zsh Setup Script with Enhanced UX and .zshenv Setup

echo "========================================="
echo "       🐚 Zsh Login Shell Setup Script   "
echo "========================================="
echo ""

# Color codes for enhanced UX
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

# Logging function for consistency
log() {
    local type=$1
    local message=$2
    local color=$NC

    case $type in
        "info") color=$GREEN; prefix="[ℹ]" ;;
        "warn") color=$YELLOW; prefix="[⚠]" ;;
        "error") color=$RED; prefix="[✖]" ;;
        *) prefix="[*]" ;;
    esac

    echo -e "${color}${prefix} ${message}${NC}"
}

# Step 1: Check if Zsh is Installed
log "info" "🔍 Step 1: Checking if Zsh is installed..."
if ! command -v zsh &> /dev/null; then
    log "error" "Zsh is not installed. Please install it with 'sudo apt install zsh' and run this script again."
    exit 1
else
    log "info" "✅ Zsh is installed."
fi
echo ""

# Step 2: Verify Current Shell and /etc/shells
log "info" "🔍 Step 2: Verifying current shell and updating /etc/shells..."
echo "Your current shell is: $SHELL"

ZSH_PATH=$(command -v zsh)
log "info" "Zsh executable located at: $ZSH_PATH"

if ! grep -Fxq "$ZSH_PATH" /etc/shells; then
    log "info" "🔧 Adding Zsh to /etc/shells..."
    echo "$ZSH_PATH" | sudo tee -a /etc/shells > /dev/null
    log "info" "✅ Zsh added to /etc/shells."
else
    log "info" "🌊 Zsh is already listed in /etc/shells."
fi
echo ""

# Step 3: Set Zsh as the Default Shell
log "info" "🔄 Step 3: Changing the default shell to Zsh for user $USER..."
sudo chsh -s "$ZSH_PATH" "$USER"

NEW_SHELL=$(getent passwd "$USER" | cut -d: -f7)
if [[ "$NEW_SHELL" == "$ZSH_PATH" ]]; then
    log "info" "✅ Default shell successfully changed to Zsh."
else
    log "error" "Failed to set Zsh as the default shell. Please try manually with 'chsh -s $(which zsh)'."
    exit 1
fi
echo ""


# Step 4: Create symlinks for .zshenv and zsh folder
log "info" "🔗 Step 4: Creating symlinks for .zshenv and zsh folder"

ZSH_ENV_PATH="$HOME/.zshenv"
ZSH_CONFIG_PATH="$HOME/.config/zsh"
DOTFILES_ZSH_PATH="$HOME/dotfiles/wsl/zsh"

# Remove existing zsh folder in $HOME/.config if present, then create the symlink
if [[ -d "$ZSH_CONFIG_PATH" ]]; then
    log "warn" "🌊 $ZSH_CONFIG_PATH already exists. Removing it to create a fresh symlink..."
    rm -rf "$ZSH_CONFIG_PATH"
    log "info" "✅ $ZSH_CONFIG_PATH removed."
fi

# Create a symbolic link for .zshenv if not already linked
if [[ ! -L "$HOME/.zshenv" ]]; then
    ln -sf "$HOME/dotfiles/wsl/zsh/.zshenv" "$ZSH_ENV_PATH" 
    log "info" "✅ Symlink created for .zshenv: $ZSH_ENV_PATH -> $HOME/dotfiles/wsl/zsh/.zshenv"
else
    log "info" "🌊 Symlink for .zshenv already exists."
fi

# Create a symbolic link for the zsh configuration folder
ln -sf "$DOTFILES_ZSH_PATH" "$ZSH_CONFIG_PATH"
log "info" "✅ Symlink created for zsh configuration folder -> $ZSH_CONFIG_PATH"
echo "🎉 Step 4 complete: All symlinks created successfully."

# Step 5: Source .zshenv & .zshrc to load into current shell session
log "info" "🔄 Sourcing .zshenv to apply changes in the current session..."
source "$ZSH_ENV_PATH"
log "info" "✅ .zshenv sourced successfully."

log "info" "🔄 Sourcing .zshrc to apply changes in the current session..."
source "$HOME/.config/zsh/.zshrc"
log "info" "✅ .zshrc sourced successfully."
echo ""

