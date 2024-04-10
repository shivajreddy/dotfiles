# This file is for listing all system apps to be installed
{ pkgs }:

let
  myPlcoud = import ./pcloud;
in 

# System packages
with pkgs; [
  discord
  webcord
  myPlcoud

  vim
  zsh
  git
  gvfs
  vlc
  streamlink
  streamlink-twitch-gui-bin
  chatterino2
  gnome.gnome-tweaks
  gnome.nautilus
  mojave-gtk-theme
  catppuccin-cursors.mochaDark

  home-manager
  openssh
]

