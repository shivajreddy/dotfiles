{ config, pkgs, ... }:
{
  imports = [];

  gtk.enable = true;

# Cursor Theme
  gtk.cursorTheme.package = pkgs.bibata-cursors;
  gtk.cursorTheme.name = "Bibata-Modern-Classic";

# Theme
  gtk.theme.package = pkgs.adw-gtk3;
  gtk.theme.name = "adw-gtk3";

# Icon Theme
  gtk.iconTheme = {
    name = "Papirus-Dark";
    package = pkgs.catppuccin-papirus-folders.override{
      accent = "flamingo"; # https://github.com/costales/folder-color/
        flavor = "mocha";
    };
  };

  gtk = {
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

        /*
        gtk-theme-name="adw-gtk3-dark";
        gtk-icon-theme-name="Adwaita";
        gtk-cursor-theme-name="Adwaita";
        gtk-cursor-theme-size=24;
        gtk-button-images=1;
        gtk-menu-images=1;
        gtk-enable-event-sounds=1;
        gtk-enable-input-feedback-sounds=0;
        gtk-xft-antialias=1;
        gtk-xft-hinting=1;
        gtk-enable-animations=true;
        gtk-primary-button-warps-slider=false;
        # */
      };
    };
    gtk4 = {
      extraConfig = {
        gtk-application-prefer-dark-theme=true;
        gtk-applications-prefer-dark-theme=true;

        /*
        gtk-cursor-theme-name="Adwaita";
        gtk-cursor-theme-size=24;
        gtk-enable-animations=true;
        gtk-icon-theme-name="Adwaita";
        gtk-primary-button-warps-slider=false;
        gtk-theme-name="adw-gtk3-dark";
        # */
      };
    };
  };


}
