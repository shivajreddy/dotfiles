
{ config, lib, pkgs, ... }:

{
  imports = [];

  programs.waybar = {
    enable = true;
  };

  # remvoing this to test locally for now
  # xdg.configFile."waybar".source = ./waybar_config;

}
