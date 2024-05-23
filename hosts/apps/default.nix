{ pkgs }:

let
  patchelfFixes = pkgs.patchelfUnstable.overrideAttrs (_finalAttrs: _previousAttrs: {
    src = pkgs.fetchFromGitHub {
      owner = "Patryk27";
      repo = "patchelf";
      rev = "527926dd9d7f1468aa12f56afe6dcc976941fedb";
      sha256 = "sha256-3I089F2kgGMidR4hntxz5CKzZh5xoiUwUsUwLFUEXqE=";
    };
  });
  pcloudFixes = pkgs.pcloud.overrideAttrs (_finalAttrs:previousAttrs: {
    nativeBuildInputs = previousAttrs.nativeBuildInputs ++ [ patchelfFixes ];
  });
in


# System packages
with pkgs; [
  home-manager

  # Hyprland related
  hyprlock
  hyprpicker
  # hyprcursor

  # Must haves
  vim
  zsh
  git
  gvfs

  # Apps
  webcord
  vlc
  pcloudFixes

  # UI
  gnome.gnome-tweaks
  gnome.nautilus  # explorer
  gnome.dconf-editor
  mojave-gtk-theme
  # catppuccin-cursors.mochaDark

  # Streaming
  # streamlink      # not working in this latest flake update
  # streamlink-twitch-gui-bin
  chatterino2

  # MISC
  openssh
  pkg-config
  openssl

]

