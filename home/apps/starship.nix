{ config, lib, pkgs, ... }:

let 
  tomlFile = builtins.readFile (./. + "/starship/starship.toml");
in {
	imports = [];

	programs.starship.enable = true;

	programs.starship.settings = builtins.fromTOML (tomlFile);
}

