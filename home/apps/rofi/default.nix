{ config, lib, pkgs, ... }:


{
	imports = [];

# this is not finished
  /*
	programs.rofi = {
	  enable = true;
	  font = "JetBrainsMono Nerd Font";
	};
  # */

  programs.rofi-wayland.enable = true;

  # xdg.configFile."WebCord/Themes/CatpuccinMocha".source = ./mocha.theme.css;
  # xdg.configFile."WebCord/Themes/CatpuccinMocha".source = ./mocha.theme.css;

  xdg.configFile."rofi/config".source = ./config.rasi;
  xdg.configFile."rofi/mocha".source = ./mocha.rasi;
}

