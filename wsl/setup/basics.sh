#!/bin/bash

# Improved Package Installation Script

# Enable error handling and quiet mode for apt commands
set -e

echo "========================================="
echo "      📦 Essential Package Installer     "
echo "========================================="
echo ""

# Step 1: Update and Upgrade Packages
echo "⌛ Step 1: Updating package list..."
sudo apt update -qq && echo "✅ Package list updated."

echo "⌛ Step 2: Upgrading existing packages..."
sudo apt upgrade -y -qq && echo "✅ System packages upgraded."
echo ""

# Step 2: Install Essential Tools
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

# Step 3: Install Neovim
echo "🌱 Step 4: Installing Neovim..."
if ! command -v nvim &> /dev/null; then
    echo "🔧 Adding Neovim PPA..."
    sudo add-apt-repository -y ppa:neovim-ppa/stable >/dev/null 2>&1
    sudo apt update -qq
    sudo apt install -y -qq neovim && echo "✅ Neovim installed successfully."
else
    echo "🌊 Neovim is already installed."
fi
echo ""

# Step 4: Install Fastfetch
echo "🌱 Step 5: Installing Fastfetch..."
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

# Completion Message
echo "========================================="
echo "😃 Installation complete! All packages are up-to-date."
echo "========================================="

