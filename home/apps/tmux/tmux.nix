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

    plugins = with pkgs; [
      tmuxPlugins.better-mouse-mode
      tmuxPlugins.vim-tmux-navigator
      {
        plugin = tmuxPlugins.catppuccin;
        extraConfig = ''
          set -g @catppuccin_falvour 'mocha'
          set -g @catppuccin_window_tabs_enabled on
        '';
      }
    ];

  };


}

