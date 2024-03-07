{ config, lib, pkgs, ... }:

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
