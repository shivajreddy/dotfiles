local wezterm = require("wezterm")

local config = wezterm.config_builder()

config.enable_wayland = false

config.audible_bell = "Disabled"

config.color_scheme = "Nocturnal Winter"

config.window_background_opacity = 0.80

-- config.font = wezterm.font("Berkeley Mono")
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
