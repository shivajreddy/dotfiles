#! /usr/bin/env bash
dots() {
    cd /home/shiva/dotfiles/
}

osbuild(){
  # echo $(hostname)
  # echo dotfiles\#$(hostname)
  sudo nixos-rebuild switch --flake ~/dotfiles\#$(hostname)
}


