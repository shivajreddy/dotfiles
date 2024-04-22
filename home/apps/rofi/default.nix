{ config, lib, pkgs, ... }:


{
	imports = [];

# this is not finished
	programs.rofi = {
	  enable = true;
	  font = "JetBrainsMono Nerd Font";
	  # theme = ''
	  # '';
	};

  xdg.configFile."rofi/config.rasi".source = ./config.rasi;
  xdg.configFile."rofi/mocha.rasi".source = ./mocha.rasi;
}

