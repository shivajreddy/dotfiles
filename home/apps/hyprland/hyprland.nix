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
	  # command = "wezterm start --class scratchpad"
	  # command = "wezterm --config-file ~/dotfiles/home/apps/terminal/wezterm/scratchpad_config.lua start --class scratchpad"


  /*
	  [scratchpads.term]
	  command = "wezterm --config-file ~/dotfiles/home/apps/terminal/wezterm/scratchpad_config.lua start --class scratchpad"
	  position = "25% 25%"
	  size = "50% 50%"
	  class = "scratchpad"
  */
}

