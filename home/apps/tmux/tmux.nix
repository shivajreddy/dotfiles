{ config, lib, pkgs, ... }:

{
  imports = [];

  programs.tmux = {
    enable = true;

    extraConfig = ''
      thm_bg="#1e1e2e"
      thm_fg="#cdd6f4"
      thm_cyan="#89dceb"
      thm_black="#181825"
      thm_gray="#313244"
      thm_magenta="#cba6f7"
      thm_pink="#f5c2e7"
      thm_red="#f38ba8"
      thm_green="#a6e3a1"
      thm_yellow="#f9e2af"
      thm_blue="#89b4fa"
      thm_orange="#fab387"
      thm_black4="#585b70"
      set -g mode-style "fg=#eee8d5,bg=#11111b"
      set -g message-style "fg=#eee8d5,bg=#073642"
      set -g message-command-style "fg=#eee8d5,bg=#073642"
      set -g pane-border-style "fg=#073642"
      set -g pane-active-border-style "fg=#eee8d5"
      set -g status "on"
      set -g status-interval 1
      set -g status-justify "left"
      set -g status-style "fg=#002b36,bg=#11111b"
      set -g status-bg "#11111b"
      set -g status-left-length "100"
      set -g status-right-length "100"
      set -g status-left-style NONE
      set -g status-right-style NONE
      set -g status-left "#[fg=#a6adc8, bg=#313244,bold] #S "
      set -g status-right "#[fg=#a6adc8, bg=#313244]   #(date '+%I:%M %p')  #h "
      setw -g window-status-activity-style "underscore,fg=#839496,bg=#fab387"
      setw -g window-status-separator ""
      setw -g window-status-style "NONE,fg=#cdd6f4"  # fg of inactive window
      setw -g window-status-format ' #{b:pane_current_path} '
      setw -g window-status-current-format '#[bg=#11111b,fg=#f5c2e7,bold] [#{b:pane_current_path}] '
      set -g default-terminal "tmux-256color"
      set -ga terminal-overrides ",xterm-256color:Tc"
      unbind C-b
      set-option -g prefix C-t
      set-option -g repeat-time 0
      set-option -g focus-events on
      set-window-option -g mode-keys vi
      bind o run-shell "open #{pane_current_path}"
      bind -r e kill-pane -a
      bind -r k select-pane -U 
      bind -r j select-pane -D 
      bind -r h select-pane -L 
      bind -r l select-pane -R 
      bind-key -n C-S-Left swap-window -t -1 \; previous-window
      bind-key -n C-S-Right swap-window -t +1 \; next-window
      bind -r C-k resize-pane -U 5
      bind -r C-j resize-pane -D 5
      bind -r C-h resize-pane -L 5
      bind -r C-l resize-pane -R 5
      set -g mouse on
      set-option -g status-justify "left"
      set-window-option -g mode-keys vi
      set-option -g status-fg cyan
      set-option -g status-bg black
      set -g pane-active-border-style fg=colour166,bg=default
      set -g window-style fg=colour10,bg=default
      set -g window-active-style fg=colour12,bg=default
      set-option -g history-limit 64096
      set -sg escape-time 10
      set-option -g status-style fg=colour146,bg=colour233,default
      # set-window-option -g window-status-style fg=colour146,bg=colour233,dim
      # set-window-option -g window-status-current-style fg=colour146,bg=colour233,default,bright
      set-option -g pane-border-style fg=colour235 #base02
      set-option -g pane-active-border-style fg=colour183,bg=colour235
      # set-option -g message-style fg=colour146,bg=colour233
      set-option -g display-panes-active-colour colour33 #blue
      set-option -g display-panes-colour colour166 #orange
      set-window-option -g clock-mode-colour colour64 #green
      set -g set-titles on
      set -g set-titles-string "#T"
      '';
  };

}

