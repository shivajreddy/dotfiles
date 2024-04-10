
# This file is for listing all system apps to be installed

{ pkgs }:
{
  imports = [
    ../apps/pcloud/default.nix
    ../apps/discord
  ];

  # System packages
  pkgs = [
    vim
    zsh 
    git
    gvfs
    vlc
    streamlink
    streamlink-twitch-gui-bin
    chatterino2 
    pcloud
    gnome.gnome-tweaks
    gnome.nautilus
    mojave-gtk-theme
    catppuccin-cursors.mochaDark

    home-manager
    openssh
  ]
}



