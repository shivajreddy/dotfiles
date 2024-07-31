local wezterm = require("wezterm")
local config = {}

-- Launching Programs
config.default_prog = { "wsl", "--cd", "/home/shiva" }

config.hide_tab_bar_if_only_one_tab = true

-- Custom Window & Tab titles
require("wezterm").on("format-window-title", function()
	return ""
end)
require("wezterm").on("format-tab-title", function()
	return ""
end)

-- Theme
config.color_scheme = "rose-pine"
config.colors = {
	background = "#111019",

	-- cursor_fg = "#e0def4",
	-- cursor_bg = "#524f67",
	cursor_fg = "#191724",
	cursor_bg = "#ebbcba",
}

config.font_size = 12.0
config.font = wezterm.font({
	family = "BerkeleyMono Nerd Font Mono Plus Font Awesome Plus Font Awesome Extension Plus Octicons Plus Power Symbols Plus Codicons Plus Pomicons Plus Font Logos Plus Material Design Icons Plus Weather Icons",
	-- comment the below line to enable ligatures
	harfbuzz_features = { "calt=0", "clig=0", "liga=0" },
})

config.font = wezterm.font_with_fallback({
	{
		family = "JetBrains Mono",
		weight = "Medium",
		harfbuzz_features = { "calt=0", "clig=0", "liga=0" },
	},
	{ family = "Terminus", weight = "Bold" },
	"Noto Color Emoji",
})

-- config.window_background_opacity = 0.93

config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}

return config
