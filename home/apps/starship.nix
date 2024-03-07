{ config, lib, pkgs, ... }:

let 
  starshipConfig = pkgs.writeTextFile {
    name = "starship.toml";
    text = builtins.readFile ./starship.toml;
  };
in
{
	imports = [];

	programs.starship.enable = true;

	programs.starship.settings = starshipConfig;
}

