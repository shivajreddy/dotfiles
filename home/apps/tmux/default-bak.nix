
{ pkgs, lib, stdenv, ... }:
let
  main_tmux_conf = builtins.readFile ./tmux.conf;
in 
{

  imports = [];

  programs.tmux = {
    enable = true;

    extraConfig = ''
      ${main_tmux_conf}
    '';



    # installing tmux plugins through nix, since TPM wont work
    plugins = with pkgs; [
      tmuxPlugins.better-mouse-mode
      tmuxPlugins.vim-tmux-navigator

      /*
      tmuxPlugins.sensible
      tmuxPlugins.yank
      tmuxPlugins.tmux-thumbs
      tmuxPlugins.tmux-fzf
      tmuxPlugins.fzf-tmux-url
      */

      {
        plugin = pkgs.fetchFromGitHub {
          owner = "catppuccin";
          repo = "tmux";
          rev = "v${version}";
          hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
        };

      }

      # /*
      {
        plugin = tmuxPlugins.catppuccin;
        extraConfig = ''
        set -g @catppuccin_flavour  "mocha"

        # set -g @catppuccin_status_default "off"
        set -g @catppuccin_status_background "default"
        '';
      }
      # */

      /*
        set -g @catppuccin_window_left_separator " "
        set -g @catppuccin_window_right_separator ""
        # set -g @catppuccin_window_left_separator "#[fg=#ffffff,bg=#ffffff,bold,underscore,italics] "
        # set -g @catppuccin_window_right_separator "#[fg=#ff0000,bg=#ff0000,bold,underscore,italics]"

        set -g @catppuccin_window_middle_separator " █"
        # set -g @catppuccin_window_middle_separator ""
        set -g @catppuccin_window_middle_separator "#[fg=#ffffff,bg=#ffffff,bold,underscore,italics]█"
        set -g @catppuccin_window_middle_separator "#[fg=#ff0000,bg=#ff0000,bold,underscore,italics]█"

        set -g @catppuccin_window_number_position "right"

        set -g @catppuccin_window_status_icon_enable "yes"

        set -g @catppuccin_window_default_fill "none" # number all none
        set -g @catppuccin_window_default_text "#W"

        set -g @catppuccin_window_current_fill "number" # number all none
        set -g @catppuccin_window_current_text "#W#{?window_zoomed_flag,  ,}"

        set -g @catppuccin_status_modules_right "directory meetings date_time"
        set -g @catppuccin_status_modules_left "session"
        set -g @catppuccin_status_left_separator  " "
        set -g @catppuccin_status_right_separator " "
        set -g @catppuccin_status_right_separator_inverse "no"
        set -g @catppuccin_status_fill "icon"
        set -g @catppuccin_status_connect_separator "no"
        set -g @catppuccin_directory_text "#{b:pane_current_path}"
        set -g @catppuccin_date_time_text "%H:%M"
      *;

      /*
      {
        plugin = tmuxPlugins.resurrect;
        extraConfig = ''
        set -g @resurrect-strategy-vim 'session'
        set -g @resurrect-strategy-nvim 'session'
        set -g @resurrect-capture-pane-contents 'on'
        '';
      }

      {
        plugin = tmuxPlugins.continuum;
        extraConfig = ''
        set -g @continuum-restore 'on'
        set -g @continuum-boot 'on'
        r
        set -g @continuum-save-interval '10'
        '';
      }
      */

    ];

  };
}