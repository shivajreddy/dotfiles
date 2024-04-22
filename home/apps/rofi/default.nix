{ config, lib, pkgs, ... }:
{
	imports = [];

  xdg.configFile."rofi/config.rasi".source = ./config.rasi;
  xdg.configFile."rofi/mocha.rasi".source = ./mocha.rasi;
}

