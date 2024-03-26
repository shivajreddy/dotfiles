{ config, lib, pkgs, ... }:

let 
  # tomlFile = builtins.readFile (./. + "/starship.toml");
  autostartConf = builtins.readFile (./. + "/autostart.conf");
  keybindsConf = builtins.readFile (./. + "/keybinds.conf");
  decorationsConf = builtins.readFile (./. + "/decorations.conf");
  workspacesConf = builtins.readFile (./. + "/workspaces.conf");

in

{
	imports = [ ];

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
        monitor=eDP-1,2560x1440@144.0, 0x0, 1.0
        monitor=DP-1,3840x2160@144.0, 2560x0,1.0
		'';
	};

  home.file.".config/hypr/pyprland.toml".text = ''
  	[pyprland]
	  plugins = ["scratchpads"]

	  [scratchpads.term]
	  command = "kitty --class scratchpad"
	  position = "25% 25%"
	  size = "50% 50%"
	  class = "scratchpad"
  '';
}

