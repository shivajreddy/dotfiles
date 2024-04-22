{ config, pkgs, ... }:

let
  myBibataMocha = import ../cursors/default.nix {inherit pkgs;};
in 
{
  imports = [];

  # this is what fixed the gnome cursors for me
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
      name = "Catppuccin-Macchiato-Compact-Blue-Dark";
      package = pkgs.catppuccin-gtk.override {
        accents = [ "blue" ];
        size = "compact";
        tweaks = [ "rimless" "black" ];
        variant = "mocha";
      };
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

    /* TODO: Test this configuration breaking dark mode for legacy applications
        gtk2 = {
          extraConfig = ''
            "gtk-theme-name=adw-gtk3-dark"
            "gtk-cursor-theme-name="Adwaita""
            "gtk-cursor-theme-size=24"
            "gtk-application-prefer-dark-theme=0"
            "gtk-applications-prefer-dark-theme=0"
            '';
        };
    # */

    gtk3 = {
      extraConfig = {
        gtk-application-prefer-dark-theme=true;
        # gtk-applications-prefer-dark-theme=true;
      };
    };

    gtk4 = {
      extraConfig = {
        gtk-application-prefer-dark-theme=true;
        # gtk-applications-prefer-dark-theme=true;
      };
    };

  };

}
