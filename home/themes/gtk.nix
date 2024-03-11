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
    gtk3 = {
      extraConfig = {
        gtk-applications-prefer-dark-theme=true;
      };
    };
    gtk4 = {
      extraConfig = {
        gtk-applications-prefer-dark-theme=true;
      };
    };
  };


}
