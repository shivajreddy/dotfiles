#!/bin/bash

# ⚠️ NOTE: Make sure to have the 'dotfiles' repo at home directory

# Enhanced Essentials Installation Script

# Enable error handling and quiet mode for apt commands
set -e

echo "========================================="
echo "      📦 Essential Package Installer     "
echo "========================================="
echo ""



# Step 1,2: Update and Upgrade Packages
echo "⌛ Step 1: Updating package list..."
sudo apt update -qq && echo "✅ Package list updated."

echo "⌛ Step 2: Upgrading existing packages..."
sudo apt upgrade -y -qq && echo "✅ System packages upgraded."
echo ""



# Step 3: Install Essential Tools
echo "🌱 Step 3: Installing essential tools..."
ESSENTIAL_PACKAGES=("software-properties-common" "git" "curl" "wget" "g++" "cmake" "pkg-config")

for package in "${ESSENTIAL_PACKAGES[@]}"; do
    if ! dpkg -s "$package" &> /dev/null; then
        sudo apt install -y -qq "$package" && echo "   - $package installed."
    else
        echo "   - $package is already installed."
    fi
done
echo ""

# Step 4: Install Zsh
echo "🌱 Step 4: Installing Zsh..."
if ! command -v zsh &> /dev/null; then
    sudo apt install -y -qq zsh && echo "✅ Zsh installed successfully."
else
    echo "🌊 Zsh is already installed."
fi
echo ""



# Step 5: Install Neovim and Set Up LazyVim Starter
echo "🌱 Step 5: Installing Neovim and Setting Up LazyVim Starter..."

# Function to install Neovim if not already installed
install_neovim() {
    if ! command -v nvim &> /dev/null; then
        echo "🔧 Adding Neovim PPA..."
        sudo add-apt-repository -y ppa:neovim-ppa/stable >/dev/null 2>&1
        echo "⏳ Updating package list..."
        sudo apt update -qq
        echo "⏳ Installing Neovim..."
        sudo apt install -y -qq neovim && echo "✅ Neovim installed successfully."
    else
        echo "🌊 Neovim is already installed."
    fi
}

install_neovim

# Clear Neovim-related cache, data, and state
echo "🚮 Removing old Neovim cache, data, and state..."
rm -rf ~/.local/share/nvim{,.bak} && echo "✅ Removed old Neovim data."
rm -rf ~/.local/state/nvim{,.bak} && echo "✅ Removed old Neovim state."
rm -rf ~/.cache/nvim{,.bak} && echo "✅ Removed old Neovim cache."

echo "🎉 Neovim setup cleanup complete! Ready to set up LazyVim."



# Step 6: Install Fastfetch
echo "🌱 Step 6: Installing Fastfetch..."
if ! command -v fastfetch &> /dev/null; then
    echo "🔧 Adding Fastfetch PPA..."
    echo "deb https://ppa.launchpadcontent.net/zhangsongcui3371/fastfetch/ubuntu jammy main" | sudo tee /etc/apt/sources.list.d/fastfetch.list >/dev/null
    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 7E2E5CB4D4865F21 >/dev/null 2>&1
    sudo apt update -qq
    sudo apt install -y -qq fastfetch && echo "✅ Fastfetch installed successfully."
else
    echo "🌊 Fastfetch is already installed."
fi
echo ""



# Step 7: Install Starship
echo "🌱 Step 7: Installing Starship prompt..."
if ! command -v starship &> /dev/null; then
    curl -sS https://starship.rs/install.sh | sh && echo "✅ Starship installed successfully."
else
    echo "🌊 Starship is already installed."
fi
echo ""



# Step 8: Install Eza
echo "🌱 Step 8: Installing Eza..."
if ! command -v eza &> /dev/null; then
    echo "🔧 Setting up Eza repository and GPG key..."
    sudo apt install -y -qq gpg
    sudo mkdir -p /etc/apt/keyrings
    wget -qO- https://raw.githubusercontent.com/eza-community/eza/main/deb.asc | sudo gpg --dearmor -o /etc/apt/keyrings/gierens.gpg
    echo "deb [signed-by=/etc/apt/keyrings/gierens.gpg] http://deb.gierens.de stable main" | sudo tee /etc/apt/sources.list.d/gierens.list >/dev/null
    sudo chmod 644 /etc/apt/keyrings/gierens.gpg /etc/apt/sources.list.d/gierens.list
    sudo apt update -qq
    sudo apt install -y -qq eza && echo "✅ Eza installed successfully."
else
    echo "🌊 Eza is already installed."
fi
echo ""

# Step 9: Make sym link to dotfiles
echo "🌱 Step 9: sym linking dotfiles to .config" 
# Create the symbolic link
ln -s ~/dotfiles/wsl ~/.config

# Confirmation message
if [ $? -eq 0 ]; then
    echo "✅ Symbolic link created successfully: ~/.config -> ~/dotfiles/common/wsl"
else
    echo "❌ Failed to create symbolic link. Please check if the paths are correct."
fi


# Completion Message
echo "========================================="
echo "😃 Installation complete! All packages are up-to-date."
echo "========================================="

