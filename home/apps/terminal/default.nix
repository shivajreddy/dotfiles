{ config, lib, pkgs, ... }:

{
	imports = [
    (./. + "/wezterm")
    # (./. + "/alacritty")
    # (./. + "/kitty")
  ];

}

