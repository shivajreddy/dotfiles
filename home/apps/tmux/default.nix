{ pkgs, lib, ... }:
let
  main_tmux_conf = builtins.readFile ./tmux.conf;
in
{
  imports = [];

  programs.tmux = {
    enable = true;

    plugins = with pkgs; [
      # tmuxPlugins.resurrect
      tmuxPlugins.better-mouse-mode
      tmuxPlugins.vim-tmux-navigator
      tmuxPlugins.tmux-sessionizer
    ];

    extraConfig = ''
      ${main_tmux_conf}
    '';
  };
}


