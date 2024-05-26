{ pkgs, lib, ... }:
let
  tmux_rose_pine = builtins.readFile ./tmux.conf;
  tmux_gruvbox = builtins.readFile ./tmux_gruvbox.conf;
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
      ${tmux_gruvbox}
    '';

  };

  # copy the scripts folder
  xdg.configFile."tmux.scripts".source = ./scripts;
  # xdg.configFile."tms".source = ./tms;

}

