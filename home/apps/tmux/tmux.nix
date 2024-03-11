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
      set -g status-right "#[fg=#a6adc8, bg=#313244]   #(date '+%H:%M')  #h "
      setw -g window-status-activity-style "underscore,fg=#839496,bg=#fab387"
      setw -g window-status-separator ""
      setw -g window-status-style "NONE,fg=#cdd6f4,bg=#11111b"  # fg of inactive window
      setw -g window-status-format ' #{b:pane_current_path} '
      setw -g window-status-current-format '#[bg=#11111b,fg=#f5c2e7,bold] [#{b:pane_current_path}] '


#set -g default-terminal "tmux-256color"
set -g default-terminal "xterm-256color"
#set -ga terminal-overrides ",*256col*:Tc"
set -ga terminal-overrides ",xterm-256color:Tc"


#### Action key
unbind C-b
set-option -g prefix C-t
set-option -g repeat-time 0
set-option -g focus-events on


#### Key bindings
set-window-option -g mode-keys vi

# bind t send-key C-t
# Reload settings
bind r source-file ~/.config/tmux/tmux.conf \; display "Reloaded!"
# Open current directory
bind o run-shell "open #{pane_current_path}"
bind -r e kill-pane -a

# vim-like pane switching
bind -r k select-pane -U 
bind -r j select-pane -D 
bind -r h select-pane -L 
bind -r l select-pane -R 

# Moving window
bind-key -n C-S-Left swap-window -t -1 \; previous-window
bind-key -n C-S-Right swap-window -t +1 \; next-window

# Resizing pane
bind -r C-k resize-pane -U 5
bind -r C-j resize-pane -D 5
bind -r C-h resize-pane -L 5
bind -r C-l resize-pane -R 5


#### basic settings
set -g mouse on

set-option -g status-justify "left"
#set-option utf8-default on
#set-option -g mouse-select-pane
set-window-option -g mode-keys vi
#set-window-option -g utf8 on
# look'n feel
set-option -g status-fg cyan
set-option -g status-bg black
set -g pane-active-border-style fg=colour166,bg=default
set -g window-style fg=colour10,bg=default
set -g window-active-style fg=colour12,bg=default
set-option -g history-limit 64096

set -sg escape-time 10

#### COLOUR

# default statusbar colors
set-option -g status-style fg=colour146,bg=colour233,default

# default window title colors
set-window-option -g window-status-style fg=colour146,bg=colour233,dim

# active window title colors
set-window-option -g window-status-current-style fg=colour146,bg=colour233,default,bright

# pane border
set-option -g pane-border-style fg=colour235 #base02
set-option -g pane-active-border-style fg=colour136,bg=colour235

# message text
set-option -g message-style fg=colour146,bg=colour233

# pane number display
set-option -g display-panes-active-colour colour33 #blue
set-option -g display-panes-colour colour166 #orange

# clock
set-window-option -g clock-mode-colour colour64 #green

# allow the title bar to adapt to whatever host you connect to
set -g set-titles on
set -g set-titles-string "#T"


source ~/.config/tmux/statusline.conf


      '';
  };

}

