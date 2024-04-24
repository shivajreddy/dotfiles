{ config, lib, pkgs, ... }:

# NOT COMPLETE
{
	programs.btop = {
		enable = true;
		extraConfig = {

		};
	};

  # xdg.configFile."kitty/themes".source = ./themes;
  xdg.configFile."btop/btop.conf".source = ./btop.conf;
  xdg.configFile."btop/mocha.conf".source = ./mocha.theme;

}
