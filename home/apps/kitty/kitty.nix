{ config, lib, pkgs, ... }:

let
  confFileContent = builtins.readFile (./. + "/kitty.conf");
in
{
	imports = [];

	programs.kitty.enable = true;
	programs.kitty.extraConfig=confFileContent;
}

