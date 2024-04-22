{ config, lib, pkgs, ... }:

{
	imports = [];

  xdg.configFile."/hypr/mocha.conf".source = ./mocha.conf;
  xdg.configFile."/hypr/hyprlock.conf".source = ./hyprlock.conf.conf;
}

