
{ config, lib, pkgs, ... }:

{
  imports = [];

  programs.waybar = {
    enable = true;
  };

  xdg.configFile."nvim/lua".source = ./lua;

}
