{ config, lib, pkgs, ... }:

let
  confFileContent = builtins.readFile (./. + "/kitty.conf");
in
{
	imports = [];

	programs.kitty.enable = true;
	programs.kitty.extraConfig=confFileContent;

  # Copy the theme folder 
  xdg.configFile."kitty/themes".source = ./themes;
}

