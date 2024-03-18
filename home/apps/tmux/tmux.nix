{ config, lib, pkgs, ... }:

{
  imports = [];

  programs.tmux = {
    enable = true;

    extraConfig = ''
      unbind C-b
      set-option -prefix C-t

      bind -r C-h select-pane -L
      bind -r C-j select-pane -D
      bind -r C-k select-pane -U
      bind -r C-l select-pane -R
    '';
  };

}

