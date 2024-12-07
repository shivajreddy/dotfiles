{ config, pkgs, ... }:

{
  imports = [];

  home.file = {
    ".config/qt5ct/qt5ct.conf".text = pkgs.lib.mkBefore (builtins.readFile (./. + "/qt5ct.conf"));
  };

  qt = {
    enable = true;
    # platformTheme = "gtk"; # gtk or gnome
    style.package = pkgs.libsForQt5.breeze-qt5;
    style.name="breeze-dark";
  };

  home.sessionVariables = {
      QT_QPA_PLATFORMTHEME="qt5ct";
  };
  
  programs.zsh.sessionVariables = {
      QT_QPA_PLATFORMTHEME="qt5ct";
  };

}
