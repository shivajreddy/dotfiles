{ config, lib, pkgs, ... }:

{
	imports = [];

	programs.wezterm.enable = true;

  # Copy the theme folder
  # xdg.configFile."kitty/themes".source = ./themes;
}

