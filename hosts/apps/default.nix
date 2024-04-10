# This file is for listing all system apps to be installed
{ pkgs }:
let
  myPcloud = import ../apps/pcloud {inherit pkgs;} ;
  myDiscord = import ../apps/discord {inherit pkgs;};
in

# System packages
with pkgs; [
  myPcloud
  myDiscord
  vim
  zsh
  git
  gvfs
  vlc
  streamlink
  streamlink-twitch-gui-bin
  chatterino2
  # pcloud
  gnome.gnome-tweaks
  gnome.nautilus
  mojave-gtk-theme
  catppuccin-cursors.mochaDark

  home-manager
  openssh
];

