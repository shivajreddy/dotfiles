#! /usr/bin/env bash

# Change to the dotfiles directory
dots() {
    cd /home/shiva/dotfiles/ || exit
}

# Rebuild NixOS configuration based on the current hostname's flake
osbuild() {
    local hostname
    hostname=$(hostname)  # Capture the hostname in a variable
    echo "Rebuilding for: $hostname"
    sudo nixos-rebuild switch --flake "~/dotfiles#$hostname"
}

# Save changes to the dotfiles repository
savedots() {
    local current_dir
    current_dir=$(pwd)  # Save the current directory
    cd ~/dotfiles || exit  # Change to dotfiles directory safely

    # Use the first positional parameter as the commit message, or use a default message if none is provided
    local commit_message="${1:-Update dotfiles}"

    git add .
    git commit -m "$commit_message"
    git push origin main

    cd "$current_dir"  # Return to the original directory
}

