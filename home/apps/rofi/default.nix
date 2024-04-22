{ config, lib, pkgs, ... }:


{
	imports = [];

  /*
	programs.rofi = {
	  enable = true;
	  font = "JetBrainsMono Nerd Font";
	};
  # */

  programs.rofi.enable = true;


  # xdg.configFile."WebCord/Themes/CatpuccinMocha".source = ./mocha.theme.css;
  # xdg.configFile."WebCord/Themes/CatpuccinMocha".source = ./mocha.theme.css;

  xdg.configFile."rofi/config.rasi".source = ./config.rasi;
  xdg.configFile."rofi/mocha.rasi".source = ./mocha.rasi;
}

