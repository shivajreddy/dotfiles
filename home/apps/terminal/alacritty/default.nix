{ config, lib, pkgs, ... }:

{
	imports = [];

	programs.alacritty.enable = true;

  # Copy the theme folder
  # xdg.configFile."kitty/themes".source = ./themes;
}

