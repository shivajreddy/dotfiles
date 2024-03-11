{ config, pkgs, ... }:
{
  imports = [];


  gtk = {

    enable = true;

    cursorTheme = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Classic";
    };

    theme = {
      # package = pkgs.adw-gtk3;
      # name = "adw-gtk3";
      package = pkgs.gnome.gnome-themes-extra;
      name = "Adwaita-dark";

    };

    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders.override{
        accent = "flamingo"; # https://github.com/costales/folder-color/
          flavor = "mocha";
      };
    };

    gtk2 = {
      extraConfig = ''
        "gtk-theme-name=adw-gtk3-dark"
        "gtk-cursor-theme-name="Adwaita""
        "gtk-cursor-theme-size=24"
        "gtk-application-prefer-dark-theme=0"
        "gtk-applications-prefer-dark-theme=0"
        '';
    };

    gtk3 = {
      extraConfig = {
        gtk-application-prefer-dark-theme=true;
        gtk-applications-prefer-dark-theme=true;
      };
    };

    gtk4 = {
      extraConfig = {
        gtk-application-prefer-dark-theme=true;
        gtk-applications-prefer-dark-theme=true;
      };
    };

  };


}
