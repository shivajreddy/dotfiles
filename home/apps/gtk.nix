{ config, pkgs, ... }:
{
  imports = [];

  /*
  home.pointerCursor.gtk.enable = true;
  home.pointerCursor.name = "Something";
  home.pointerCursor.package = pkgs.catppuccin-cursors.mochaSapphire;
  # */

  /*
  home.packages = with pkgs; [
    apple_cursor
  ];
  # */

/*
  home.pointerCursor = 
    let 
      getFrom = url: hash: name: {
          gtk.enable = true;
          x11.enable = true;
          name = name;
          size = 24;
          package = 
            pkgs.runCommand "moveUp" {} ''
              mkdir -p $out/share/icons
              ln -s ${pkgs.fetchzip {
                url = url;
                hash = hash;
              }} $out/share/icons/${name}
          '';
        };
    in
      getFrom 
        "https://github.com/ful1e5/fuchsia-cursor/releases/download/v2.0.0/Fuchsia-Pop.tar.gz"
        "sha256-BvVE9qupMjw7JRqFUj1J0a4ys6kc9fOLBPx2bGaapTk="
        "Fuchsia-Pop";
*/

  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders.override{
        accent = "blue"; # https://github.com/costales/folder-color/
	flavor = "mocha";
      };
    };
    theme = {
      name = "Mocha-Sapphire-Dark";
      package = pkgs.catppuccin-gtk.override {
	accents = [ "sapphire" ];
	size = "standard";
	tweaks = [ "rimless" "black" ];
	variant = "mocha";
      };
    };
    #/*
    cursorTheme = {
      name = "Catppuccin-Mocha-Dark-Sapphire";
      # https://github.com/catppuccin/cursors
      package = pkgs.catppuccin-cursors.mochaSapphire;
      # package = pkgs.mcmojave-cursors;
    };
    # */
 
    gtk3 = {
      extraConfig = {
	gtk-application-prefer-dark-theme=1;
      };
    };
    gtk4 = {
      extraConfig = {
	gtk-application-prefer-dark-theme=1;
      };
    };
  };


  # /* Linking the Theme assets
  xdg.configFile = {
    #"gtk-4.0/assets".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
    "gtk-4.0/assets" = {
      source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
      recursive = true;
    };
    "gtk-4.0/gtk.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
    "gtk-4.0/gtk-dark.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
  };
  # */

  /* Remove existing GTK settings to avoid conflicts
  home.file.".config/gtk-3.0/settings.ini".state = "absent";
  home.file.".config/gtk-4.0/settings.ini".state = "absent";
  # */

  /* After ensuring the existing settings are absent, recreate them with the desired theme settings
  home.file.".config/gtk-3.0/settings.ini".text = ''
    [Settings]
    gtk-theme-name="${config.gtk.theme.name}"
    gtk-icon-theme-name="YourIconTheme" # Replace with your icon theme
    gtk-font-name="YourFont 11" # Replace with your font settings
  '';
  home.file.".config/gtk-4.0/settings.ini".text = ''
    [Settings]
    gtk-theme-name="${config.gtk.theme.name}"
    gtk-icon-theme-name="YourIconTheme" # Replace with your icon theme
    gtk-font-name="YourFont 11" # Replace with your font settings
  '';
  # */

}
