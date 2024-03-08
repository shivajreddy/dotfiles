
{ config, lib, pkgs, ... }:

let
  tomlFileContent = builtins.readFile (./. + "/alacritty.toml");
in
{
	imports = [];

	programs.alacritty.enable = true;
	programs.alacritty.extraConfig=tomlFileContent;
}



