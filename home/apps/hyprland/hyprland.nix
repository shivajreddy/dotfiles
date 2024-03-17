{ config, lib, pkgs, ... }:

let 
  # tomlFile = builtins.readFile (./. + "/starship.toml");
  autostartConf = builtins.readFile (./. + "/autostart.conf");
  keybindsConf = builtins.readFile (./. + "/keybinds.conf");
  decorationsConf = builtins.readFile (./. + "/decorations.conf");
  workspacesConf = builtins.readFile (./. + "/workspaces.conf");

  m1 = builtins.getEnv "MY_MONITOR_1_DECORATION";
  m2 = builtins.getEnv "MY_MONITOR_2_DECORATION";

  myMonitors = ''
  ${builtins.getEnv "MY_MONITOR_1_DECORATION"}
  ${builtins.getEnv "MY_MONITOR_2_DECORATION"}
  '';
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
		'';
	};
    # ${myMonitors}

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

