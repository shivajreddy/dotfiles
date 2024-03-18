{ config, lib, pkgs, ... }:

{
  imports = [];

  programs.tmux = {
    enable = true;

    extraConfig = ''
      unbind C-b
      set-option -prefix C-t

      bind -n C-h select-pane -L
      bind -n C-j select-pane -D
      bind -n C-k select-pane -U
      bind -n C-l select-pane -R
    '';
  };

}

