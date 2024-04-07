
# This file is for listing all system apps to be installed

{ pkgs }:

# System packages
with pkgs; [
  vim
  zsh 
  git
  gvfs
  vlc
  streamlink
  streamlink-twitch-gui-bin
  # chatterino2 # moving to home-manager
  pcloud
  gnome.gnome-tweaks
  gnome.nautilus
  mojave-gtk-theme
  catppuccin-cursors.mochaDark

  home-manager
  openssh
]


