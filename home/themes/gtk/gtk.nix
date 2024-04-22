{ config, pkgs, ... }:

let
  myBibataMocha = import ../cursors/default.nix {inherit pkgs;};

  my_catppuccin_name = "Catppuccin-Mocha-Standard-Blue-Dark";
  my_catppuccin = pkgs.catppuccin-gtk.override {
        accents = [ "blue" ];
        size = "standard";
        tweaks = [ "normal" ];
        variant = "mocha";
  };
  /*
  my_catppuccin_name = "Catppuccin-Mocha-Standard-Pink-Dark";
  my_catppuccin = pkgs.catppuccin-gtk.override {
        accents = [ "blue" ];
        accents = [ "blue" ];
        size = "standard";
        tweaks = [ "normal" "rimless"];
        variant = "mocha";
  };
  */
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
      };
    };
  };


  # useful link: https://github.com/catppuccin/gtk/issues/110
  home.sessionVariables.GTK_THEME = "${my_catppuccin_name}";
  # home.file.".config/gtk-4.0/gtk.css".source = "${my_catppuccin}/share/themes/${my_catppuccin_name}/gtk-4.0/gtk.css";

  /*
  home.file.".config/gtk-4.0/gtk-dark.css".source = "${my_catppuccin}/share/themes/${my_catppuccin_name}/gtk-4.0/gtk-dark.css";
  home.file.".config/gtk-4.0/assets" = {
    recursive = true;
    source = "${my_catppuccin}/share/themes/${my_catppuccin_name}/gtk-4.0/assets";
  };
  # */

}
