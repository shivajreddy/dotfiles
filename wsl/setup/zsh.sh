#!/bin/bash

echo "Step 1: Adding Zsh to /etc/shells if not already present."

# Check if Zsh is installed
if ! command -v zsh &> /dev/null; then
    echo "Zsh is not installed. Please install it first, then run this script again."
    exit 1
fi

# Locate the Zsh executable
ZSH_PATH=$(command -v zsh)

# Display the current shell for reference
echo "Current shell: $SHELL"

# Check if Zsh is already in /etc/shells
if ! grep -Fxq "$ZSH_PATH" /etc/shells; then
    echo "Adding $ZSH_PATH to /etc/shells..."
    echo "$ZSH_PATH" | sudo tee -a /etc/shells
else
    echo "Zsh is already listed in /etc/shells."
fi

echo "Step 2: Changing the default shell to Zsh."

# Change the default shell to Zsh
sudo chsh -s "$ZSH_PATH" "$USER"

# Confirm the default shell change
echo "Your default shell is now set to:"
echo "$SHELL"

echo "Script completed. Please open a new terminal session to start using Zsh."

