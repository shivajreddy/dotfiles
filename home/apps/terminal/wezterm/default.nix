{ config, lib, pkgs, ... }:

{
	imports = [];

	programs.wezterm.enable = true;

  # Copy the theme folder
  # xdg.configFile."kitty/themes".source = ./themes;

  programs.wezterm.extraConfig = ''
  
    -- Pull in the wezterm API

    local wezterm = require("wezterm")

    -- This will hold the configuration.

    local config = wezterm.config_builder()

    config.enable_wayland = false

    config.default_prog = { "tmux" }

    config.audible_bell = "Disabled"

    config.color_scheme = "rose-pine"

    config.font = wezterm.font("IBM Plex Mono")
    config.font_size = 14

    config.use_fancy_tab_bar = false
    config.enable_tab_bar = false

    config.window_padding = {
      top = 0,
      right = 0,
      bottom = 0,
      left = 0,
    }

    -- and finally, return the configuration to wezterm
    return config
  '';
}

