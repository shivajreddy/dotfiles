{ config, lib, pkgs, ... }:

{
	imports = [];

  xdg.configFile."/hypr/mocha.conf".source = ./mocha.conf;
}

