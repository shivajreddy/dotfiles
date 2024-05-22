{ pkgs, lib, ... }:
let
  main_tmux_conf = builtins.readFile ./tmux.conf;
in
{
  imports = [];

  programs.tmux = {
    enable = true;

    plugins = with pkgs; [
      tmuxPlugins.resurrect
      tmuxPlugins.continuum
      tmuxPlugins.better-mouse-mode
      tmuxPlugins.vim-tmux-navigator
      # tmux-sessionizer is not a plugin, its a package, installed on home/default.nix
    ];

    extraConfig = ''
      ${main_tmux_conf}
    '';

  };

  # xdg.configFile."tms".source = ./tms;

}

