#!/bin/bash

# Zsh Setup Script with Enhanced UX and .zshenv Setup

echo "========================================="
echo "       🐚 Zsh Login Shell Setup Script   "
echo "========================================="
echo ""

# Step 1: Check if Zsh is Installed
echo "🔍 Step 1: Checking if Zsh is installed..."
if ! command -v zsh &> /dev/null; then
    echo "⚠️  Zsh is not installed. Please install it with 'sudo apt install zsh' and run this script again."
    exit 1
else
    echo "✅ Zsh is installed."
fi
echo ""

# Step 2: Verify Current Shell and /etc/shells
echo "🔍 Step 2: Verifying current shell and updating /etc/shells..."

# Show current shell
echo "Your current shell is: $SHELL"

# Check if Zsh is already in /etc/shells
ZSH_PATH=$(command -v zsh)
echo "Zsh executable located at: $ZSH_PATH"

if ! grep -Fxq "$ZSH_PATH" /etc/shells; then
    echo "🔧 Adding Zsh to /etc/shells..."
    echo "$ZSH_PATH" | sudo tee -a /etc/shells > /dev/null
    echo "✅ Zsh added to /etc/shells."
else
    echo "🌊 Zsh is already listed in /etc/shells."
fi
echo ""

# Step 3: Set Zsh as the Default Shell
echo "🔄 Step 3: Changing the default shell to Zsh for user $USER..."

# Change the default shell
sudo chsh -s "$ZSH_PATH" "$USER"

# Confirm the change
NEW_SHELL=$(getent passwd "$USER" | cut -d: -f7)
if [[ "$NEW_SHELL" == "$ZSH_PATH" ]]; then
    echo "✅ Default shell successfully changed to Zsh."
else
    echo "❌ Failed to set Zsh as the default shell. Please try manually with 'chsh -s $(which zsh)'."
    exit 1
fi
echo ""


# Step 4: Create .zshenv in Home Directory and Source It
echo "📄 Step 4: Creating .zshenv file in the home directory (if not already present)..."
ZSHENV_PATH="$HOME/.zshenv"
if [[ ! -f "$ZSHENV_PATH" ]]; then
    echo "export ZDOTDIR=~/.config/zsh" > "$ZSHENV_PATH"
    echo "✅ .zshenv created with 'export ZDOTDIR=~/.config/zsh'."
else
    echo "🌊 .zshenv already exists. No changes made."
fi

# Step 5: Source .zshenv to load into current shell session
echo "🔄 Sourcing .zshenv to apply changes in the current session..."
source "$ZSHENV_PATH"
echo "✅ .zshenv sourced successfully."
echo ""
# Source .zshrc to load into current shell session
echo "🔄 Sourcing .zshrc to apply changes in the current session..."
source "$HOME/.config/zsh/.zshrc"
echo "✅ .zshrc sourced successfully."
echo ""


# Step 6: Final Instructions
echo "🎉 Zsh setup is complete!"
echo "To start using Zsh, please open a new terminal session."
echo "You can verify by running 'echo \$SHELL' in the new session."
echo "========================================="

