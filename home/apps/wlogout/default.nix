{ ... }:

{

	programs.wlogout.enable = true;

  # Copy the theme folder 
  xdg.configFile."wlogout/icons".source = ./icons;
  xdg.configFile."wlogout/layout".source = ./layout;
  xdg.configFile."wlogout/style.css".source = ./style.css;
}

