#! /usr/bin/env bash

# Rebuild NixOS configuration based on the current hostname's flake
osbuild() {
    local hostname
    hostname=$(hostname)  # Capture the hostname in a variable
    echo "NixOS-Rebuild for: $hostname"
    sudo nixos-rebuild switch --flake "$HOME/dotfiles#$hostname"
}
