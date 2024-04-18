local wezterm = require("wezterm")

local config = wezterm.config_builder()

config.enable_wayland = false

config.color_scheme = "Github Dark (Gogh)"

config.window_background_opacity = 0.95

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

return config
