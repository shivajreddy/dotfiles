{ config, lib, pkgs, ... }:

{
	imports = [];

	programs.hyprlock.enable = true;

  xdg.configFile."/hypr/mocha.conf".source = ./mocha.conf;
}

