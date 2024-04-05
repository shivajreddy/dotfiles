#! /usr/bin/env bash
dots() {
    cd /home/shiva/dotfiles/
}

osbuild(){
  # sudo nixos-rebuild switch --flake ~dotfiles$1
  echo $(hostname)
}


