{ config, lib, pkgs, ... }:

let 
  	# tomlFile = builtins.readFile (./. + "/starship.toml");
 	autostartConf = builtins.readFile (./. + "/autostart.conf");
	keybindsConf = builtins.readFile (./. + "/keybinds.conf");
 	decorationsConf = builtins.readFile (./. + "/decorations.conf");
	workspacesConf = builtins.readFile (./. + "/workspaces.conf");
in

{
	imports = [
	];
	wayland.windowManager.hyprland = {
		enable = true;
		plugins = [];
		settings = {};
		xwayland = { enable = true; };
		systemd.enable = true;
	        extraConfig = ''
		${autostartConf}
		${keybindsConf}
		${decorationsConf}
		${workspacesConf}
		'';
	};

  	home.file.".config/hypr/pyprland.toml".source = ./pyprland.toml;
}

