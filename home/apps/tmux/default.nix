{ pkgs, lib, ... }:
let
  main_tmux_conf = builtins.readFile ./tmux.conf;
in
{
  imports = [];

  programs.tmux = {
    enable = true;

    plugins = with pkgs; [
      tmuxPlugins.better-mouse-mode
      tmuxPlugins.vim-tmux-navigator
    ];

    extraConfig = ''
      ${main_tmux_conf}
    '';
  };
}

