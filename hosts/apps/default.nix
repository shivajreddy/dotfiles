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
  webcord
  pcloudFixes

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

