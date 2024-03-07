# ~/.config/nixpkgs/pyprland.nix

{ config, pkgs, ... }:

{
  home.packages = [
    # Add any Python packages you want to install globally here
    pkgs.python
  ];

  programs.pyprland = {
    enable = true;
    extraConfig = ''
      # Add your Pyprland configurations here
	[pyprland]
	plugins = ["scratchpads", "magnify"]

	[scratchpads.term]
	command = "kitty --class scratchpad"
	class = "scratchpad"
	size = "75% 60%"
	max_size = "800px 40%"
	margin = 50
    '';
  };
}

