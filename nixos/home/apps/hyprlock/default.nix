{ config, lib, pkgs, ... }:

{
	imports = [];

  # xdg.configFile."/hypr/mocha.conf".source = ./mocha.conf;
  # xdg.configFile."/hypr/hyprlock.conf".source = ./hyprlock.conf;

  home.file.".config/hypr/mocha.conf".source = ./mocha.conf;
  home.file.".config/hypr/hyprlock.conf".source = ./hyprlock.conf;

}

