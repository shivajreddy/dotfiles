{ config, pkgs, ... }:
{
  imports = [];

  # Thsi fixes legacy applications
  home.pointerCursor = {
    gtk.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
  };


  gtk = {

    enable = true;

    # Desktop Theme
    theme = {
      /* 
      package = pkgs.adw-gtk3;
      name = "adw-gtk3";
      # */

      /* This fixed dark mode for every app
      package = pkgs.gnome.gnome-themes-extra;
      name = "Adwaita-dark";
      # */

      # /* Catppuccin Theme -> https://github.com/catppuccin/gtk
      name = "Mocha-Pink"
      package = pkgs.catppuccin-gtk.override {
        accents = [ "pink" ];
        size = "compact";
        tweaks = [ "rimless" ];
        variant = "mocha";
      };
      # */ 
    };

    # /* Catppuccin: Now symlink the `~/.config/gtk-4.0/` folder declaratively:
    xdg.configFile = {
      "gtk-4.0/assets".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
      "gtk-4.0/gtk.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
      "gtk-4.0/gtk-dark.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
    };
    # */

    # Cursors
    cursorTheme = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Classic";
    };

    # Icons
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders.override{
        accent = "flamingo"; # https://github.com/costales/folder-color/
          flavor = "mocha";
      };
    };

    gtk2 = {
      /* TODO: Test this configuration breaking dark mode for legacy applications
      extraConfig = ''
        "gtk-theme-name=adw-gtk3-dark"
        "gtk-cursor-theme-name="Adwaita""
        "gtk-cursor-theme-size=24"
        "gtk-application-prefer-dark-theme=0"
        "gtk-applications-prefer-dark-theme=0"
        '';
      # */
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
