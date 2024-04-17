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

          set -g @catppuccin_window_right_separator "█ "
          set -g @catppuccin_window_number_position "right"
          set -g @catppuccin_window_middle_separator " | "
          set -g @catppuccin_window_default_fill "none"
          set -g @catppuccin_window_current_fill "all"
          set -g @catppuccin_status_modules_right "application session user host date_time"
          set -g @catppuccin_status_left_separator "█"
          set -g @catppuccin_status_right_separator "█"
          set -g @catppuccin_date_time_text "%Y-%m-%d %H:%M:%S"
        '';
      }
    ];

  };
}
