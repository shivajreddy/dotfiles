{ config, lib, pkgs, ... }:


{
	imports = [];

# this is not finished
	programs.rofi = {
	  enable = true;
	  font = "JetBrains Mono";
	  theme = ''
	  '';
	};
}

