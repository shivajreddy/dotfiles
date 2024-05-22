{ pkgs, ... }:

let
  # 4.1: import the cursor theme that we created
  # myBibataMocha = import ../cursors/default.nix {inherit pkgs;};
  bibataRosePine = import ../cursors/default.nix {inherit pkgs;};

  my_catppuccin_name = "Catppuccin-Mocha-Standard-Lavender-Dark";
  my_catppuccin = pkgs.catppuccin-gtk.override {
        accents = [ "lavender" ];
        size = "standard";
        tweaks = [ "normal" ];
        variant = "mocha";
  };
in 
{
  imports = [];

  # this is what fixed the gnome cursors for me.
  home.pointerCursor = {
    gtk.enable = true;

    # 4.2: Set the name here or else it wont work
    package = bibataRosePine;
    name = "Bibata-RosePine";
  };


  gtk = {

    enable = true;

    # MAIN THEME
    theme = {
      name = my_catppuccin_name;
      package = my_catppuccin;
    };

    # Cursors
    cursorTheme = {
      # 5. Finally set it here too
      package = bibataRosePine;
      name = "Bibata-RosePine";
      # package = myBibataMocha;
      # name = "Bibata-Mocha-Blue";
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

}
