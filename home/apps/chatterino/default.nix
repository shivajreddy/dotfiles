{ config, lib, pkgs, ... }:

{

  # install the system package: chatterino2
  inputs.pkgs = [
    chatterino2
  ];
  /*
  with pkgs; [
    chatterino2
  ];
  */

	# programs.chatterino2.enable = true;

  # xdg.configFile."swaync".source = ../swaync/swaync_config;
  # copy the settings folder into .local/share/chatterino2
  home.file.".local/share/catterino2/Settings".source = ./config/Settings;

  # copy the themes folder into .local/share/chatterino2
  home.file.".local/share/catterino2/Themes".source = ./config/Themes;

}

