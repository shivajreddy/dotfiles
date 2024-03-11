{ config, pkgs, ... }:

{
  imports = [];

  # Enable qt
  qt.enable = true;

  # platform theme "gtk" or "gnome"
  qt.platformTheme = "gtk";

  # name of the qt theme
  qt.style.name = "adwaita-dark";

  qt.style.package = pkgs.adwaita-qt;

}
