{ config, pkgs, ... }:

let
  myBibataMocha = import ../cursors/default.nix {inherit pkgs;};

  my_catppuccin_name = "Catppuccin-Macchiato-Standard-Green-Dark";
  my_catppuccin = pkgs.catppuccin-gtk.override {
        accents = [ "green" ];
        size = "standard";
        tweaks = [ "normal" ];
        variant = "macchiato";
  };
in 
{
  imports = [];

  # this is what fixed the gnome cursors for me.
  home.pointerCursor = {
    gtk.enable = true;
    # package = pkgs.bibata-cursors;
    # name = "Bibata-Modern-Classic";

    # 4. Set the name here or else it wont work
    package = myBibataMocha;
    name = "Bibata-Mocha-Blue";
  };


  gtk = {

    enable = true;

    # Desktop Theme
    /*
    theme = {
      package = pkgs.gnome.gnome-themes-extra;
      name = "Adwaita-dark";
    };
    */
    theme = {
      name = my_catppuccin_name;
      package = my_catppuccin;
    };

    # Cursors
    cursorTheme = {
      # package = pkgs.bibata-cursors;
      # name = "Bibata-Modern-Classic";

    # 5. Finally set it here too
      package = myBibataMocha;
      name = "Bibata-Mocha-Blue";
    };

    # Icons
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders.override{
        accent = "blue"; # https://github.com/costales/folder-color/
          flavor = "mocha";
      };
    };

    gtk3 = {
      extraConfig = {
        gtk-application-prefer-dark-theme=true;
        # gtk-applications-prefer-dark-theme=true;
      };
    };

/*
    gtk4 = {
      extraConfig = {
        gtk-application-prefer-dark-theme=true;
      };
    };
    */

  };

  # home.file.".config/gtk-4.0/gtk.css".source = "${my_catppuccin}/share/themes/${my_catppuccin_name}/gtk-4.0/gtk.css";
  home.file.".config/gtk-4.0/gtk-dark.css".source = "${my_catppuccin}/share/themes/${my_catppuccin_name}/gtk-4.0/gtk-dark.css";
  home.file.".config/gtk-4.0/assets" = {
    recursive = true;
    source = "${my_catppuccin}/share/themes/${my_catppuccin_name}/gtk-4.0/assets";
  };
  /*
  # */

  /* Now symlink the `~/.config/gtk-4.0/` folder declaratively
  xdg.configFile = {
    "gtk-4.0/assets".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
    "gtk-4.0/gtk.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
    "gtk-4.0/gtk-dark.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
  };
  # */

}
