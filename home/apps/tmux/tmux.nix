{ config, lib, pkgs, ... }:

{
  imports = [];

  programs.tmux = {
    enable = true;

    extraConfig = ''
    '';
  };

}

