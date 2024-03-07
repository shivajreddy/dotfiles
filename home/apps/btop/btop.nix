{ config, lib, pkgs, ... }:

# NOT COMPLETE
{
	programs.btop = {
		enable = true;
		extraConfig = {

		};
	};

	home.sessionVariable ={
	 XDG_CONFIG_HOME = ./.
	};
}
