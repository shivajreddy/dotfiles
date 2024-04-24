{ config, lib, pkgs, ... }:

# NOT COMPLETE
{
	programs.btop = {
		enable = true;

    color_theme = "mocha";
    theme_background = false;
		extraConfig = {
		};
	};

  xdg.configFile."btop/btop.conf".source = ./btop.conf;
  xdg.configFile."btop/themes".source = ./themes;

}
