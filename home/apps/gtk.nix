{ config, pkgs, ... }:
{
  imports = [];

  gtk = {
    enable = true;
    theme = {
      name = "Mocha-Sapphire-Dark";
      package = pkgs.catppuccin-gtk.override {
	accents = [ "sapphire" ];
	size = "standard";
	tweaks = [ "rimless" ];
	variant = "mocha";
      };
    };
    cursorTheme = {
      name = "Catppuccin-Mocha-Dark-Sapphire";
      # https://github.com/catppuccin/cursors
      package = pkgs.catppuccin-cursors.mochaSapphire;
    };
    iconTheme = {
      name = "Catppuccin-Mocha-Dark-Cursors";
      #package = pkgs.catppuccin-
    };
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
    "gtk-4.0/assets".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
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
