{ config, lib, pkgs, ... }:

let
  tmux-conf = builtins.readFile ./tmux.conf;
in 
{
  imports = [];

  programs.tmux = {
    enable = true;

    extraConfig = ''
    ${tmux-conf}
    '';
  };

}

