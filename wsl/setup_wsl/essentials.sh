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
ESSENTIAL_PACKAGES=("fuse" "libfuse2" "software-properties-common" "vim" "git" "gh" "curl" "wget" "g++" "cmake" "pkg-config" "zoxide")

for package in "${ESSENTIAL_PACKAGES[@]}"; do
	if ! dpkg -s "$package" &>/dev/null; then
		sudo apt install -y -qq "$package" && echo "   - $package installed."
	else
		echo "   - $package is already installed."
	fi
done
echo ""

# Step 4: Install Zsh
echo "🌱 Step 4: Installing Zsh..."
if ! command -v zsh &>/dev/null; then
	sudo apt install -y -qq zsh && echo "✅ Zsh installed successfully."
else
	echo "🌊 Zsh is already installed."
fi
echo ""

# Step 5: Install Fastfetch
echo "🌱 Step 6: Installing Fastfetch..."
if ! command -v fastfetch &>/dev/null; then
	echo "🔧 Adding Fastfetch PPA..."
	echo "deb https://ppa.launchpadcontent.net/zhangsongcui3371/fastfetch/ubuntu jammy main" | sudo tee /etc/apt/sources.list.d/fastfetch.list >/dev/null
	sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 7E2E5CB4D4865F21 >/dev/null 2>&1
	sudo apt update -qq
	sudo apt install -y -qq fastfetch && echo "✅ Fastfetch installed successfully."
else
	echo "🌊 Fastfetch is already installed."
fi
echo ""

# Step 6: Install Starship
echo "🌱 Step 6: Installing Starship prompt..."
if ! command -v starship &>/dev/null; then
	curl -sS https://starship.rs/install.sh | sh && echo "✅ Starship installed successfully."
else
	echo "🌊 Starship is already installed."
fi
echo ""

# Step 7: Install Eza
echo "🌱 Step 7: Installing Eza..."
if ! command -v eza &>/dev/null; then
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

# Step 8: Install lf
echo "🌱 Step 8: Installing lf (a terminal file manager)..."
if ! command -v lf &>/dev/null; then
	echo "🔧 Downloading and installing lf..."
	wget -qO- https://github.com/gokcehan/lf/releases/latest/download/lf-linux-amd64.tar.gz | tar -xz
	sudo mv lf /usr/local/bin/
	echo "✅ lf installed successfully."
else
	echo "🌊 lf is already installed."
fi
echo ""

# Completion Message
echo "========================================="
echo "😃 Installation complete! All packages are up-to-date."
echo "========================================="
