#!/bin/bash

# Improved Zsh Setup Script

echo "========================================="
echo "     Zsh Setup and Default Shell Switch  "
echo "========================================="
echo ""

# Step 1: Check if Zsh is Installed
echo "Checking if Zsh is installed..."
if ! command -v zsh &> /dev/null; then
    echo "⚠️  Zsh is not installed. Please install it first (e.g., 'sudo apt install zsh'), then run this script again."
    exit 1
else
    echo "✅ Zsh is installed."
fi
echo ""

# Step 2: Locate Zsh and Check /etc/shells
ZSH_PATH=$(command -v zsh)
echo "Zsh location: $ZSH_PATH"

echo "Checking if Zsh is listed in /etc/shells..."
if ! grep -Fxq "$ZSH_PATH" /etc/shells; then
    echo "⚠️  Zsh is not in /etc/shells. Adding it now..."
    echo "$ZSH_PATH" | sudo tee -a /etc/shells > /dev/null
    echo "✅ Zsh has been added to /etc/shells."
else
    echo "✅ Zsh is already listed in /etc/shells."
fi
echo ""

# Step 3: Display Current Shell and Prompt for Change
echo "Current default shell: $SHELL"
read -p "Would you like to switch your default shell to Zsh? (y/n): " -n 1 -r
echo ""

if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Switching default shell to Zsh for user $USER..."
    sudo chsh -s "$ZSH_PATH" "$USER"
    echo "✅ Default shell changed to Zsh."
else
    echo "❌ Default shell change canceled by user."
    exit 0
fi
echo ""

# Step 4: Confirm and Prompt for Restart
echo "To start using Zsh, please open a new terminal session."
echo "You can verify by running 'echo \$SHELL' in the new session."
echo "Script completed successfully! 🎉"
echo "========================================="

