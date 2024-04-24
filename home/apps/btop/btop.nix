{ config, lib, pkgs, ... }:

# NOT COMPLETE
{
	programs.btop = {
		enable = true;
		extraConfig = {

		};
	};

  xdg.configFile."kitty/themes".source = ./themes;

	home.sessionVariable ={
	 XDG_CONFIG_HOME = ./.
	};
}
