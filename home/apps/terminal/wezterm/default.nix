{ config, lib, pkgs, ... }:


let
  weztermConfig = builtins.readFile (./main_config.lua);
in 

{
	imports = [];

	programs.wezterm.enable = true;

  programs.wezterm.extraConfig = ''
  ${weztermConfig}
  '';
}

