#! /usr/bin/env bash
dots() {
    cd /home/shiva/dotfiles/
}

osbuild(){
  # echo $(hostname)
  # sudo nixos-rebuild switch --flake ~dotfiles\#$()
  echo dotfiles\#$(hostname)
}


