{ config, lib, pkgs, ... }:

{
	imports = [
    (./. + "/alacritty")
    # (./. + "/kitty")
    # (./. + "/wezterm")
  ];

}

