{ config, lib, pkgs, ... }:
{
	imports = [];

  programs.rofi.enable = true;

  xdg.configFile."rofi/config.rasi".source = ./config.rasi;
  xdg.configFile."rofi/mocha.rasi".source = ./mocha.rasi;
}

