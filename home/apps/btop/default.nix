{ config, lib, pkgs, ... }:

# NOT COMPLETE
{
	programs.btop = {
		enable = true;
		extraConfig = {

		};
	};

  xdg.configFile."btop/btop.conf".source = ./btop.conf;
  xdg.configFile."btop/themes".source = ./themes;

}
