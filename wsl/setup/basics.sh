#!/bin/bash

echo "Starting setup for basic necessary packages..."

# Update package lists
sudo apt update

# Install basic utilities
echo "Installing basic utilities..."
sudo apt install -y curl wget git build-essential software-properties-common apt-transport-https

# Install Neovim
echo "Installing Neovim..."
sudo add-apt-repository ppa:neovim-ppa/stable -y
sudo apt update
sudo apt install -y neovim

# Install fastfetch from a PPA if on Ubuntu, or manually on Debian
if grep -q "Ubuntu" /etc/os-release; then
    echo "Installing fastfetch from PPA on Ubuntu..."
    sudo add-apt-repository ppa:zhangsongcui3371/fastfetch -y
    sudo apt update
    sudo apt install -y fastfetch
else
    echo "Installing fastfetch manually on Debian..."
    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 7E2E5CB4D4865F21
    echo "deb https://ppa.launchpadcontent.net/zhangsongcui3371/fastfetch/ubuntu jammy main" | sudo tee /etc/apt/sources.list.d/fastfetch.list
    sudo apt update
    sudo apt install -y fastfetch
fi

# Install other development tools
echo "Installing development tools..."
sudo apt install -y cmake g++ pkg-config

# Additional configuration for Neovim
echo "Configuring Neovim..."
mkdir -p ~/.config/nvim
echo 'set number' > ~/.config/nvim/init.vim

echo "Setup complete! All basic packages have been installed."

