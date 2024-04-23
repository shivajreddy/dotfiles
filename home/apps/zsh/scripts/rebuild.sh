#! /usr/bin/env bash

# Rebuild NixOS configuration based on the current hostname's flake
rebuild() {
    local hostname
    hostname=$(hostname)  # Capture the hostname in a variable
    echo "Home-Manager-Rebuild for: $hostname"

    home-manager switch --flake "$HOME/dotfiles"
}

