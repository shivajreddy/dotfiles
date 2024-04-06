{ config, lib, pkgs, ... }:

{
  imports = [];

  programs.waybar = {
    enable = true;
  };

  xdg.configFile."waybar".source = ./waybar_config;
  xdg.configFile."swaync".source = ../swaync/swaync_config;

}
