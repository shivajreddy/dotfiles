local wezterm = require("wezterm")
local config = {}

-- Launching Programs
config.default_prog = { "wsl", "--cd", "/home/shiva" }
-- config.default_prog = { "zellij" }

config.hide_tab_bar_if_only_one_tab = true

-- Custom Window & Tab titles
-- config.window_decorations = "RESIZE"
config.window_decorations = "TITLE | RESIZE"
require("wezterm").on("format-window-title", function()
	-- return "simple_colon"
	return ""
end)
require("wezterm").on("format-tab-title", function()
	return ""
end)

-- Theme
-- config.color_scheme = "rose-pine"
config.color_scheme = "catppuccin-mocha"
config.colors = {
	background = "#111019",
	-- cursor_fg = "#e0def4",
	-- cursor_bg = "#524f67",
	cursor_fg = "#191724",
	cursor_bg = "#ebbcba",
}

config.font_size = 12.0

config.font = wezterm.font_with_fallback({
	{
		family = "BerkeleyMono Nerd Font Mono Plus Font Awesome Plus Font Awesome Extension Plus Octicons Plus Power Symbols Plus Codicons Plus Pomicons Plus Font Logos Plus Material Design Icons Plus Weather Icons",
		-- comment the below line to enable ligatures
		harfbuzz_features = { "calt=0", "clig=0", "liga=0" },
	},
	{ family = "JetBrains Mono", weight = "Bold" },
	{ family = "Terminus", weight = "Bold" },
	"Noto Color Emoji",
})

-- config.window_background_opacity = 0.93

config.window_padding = {
	left = 10,
	right = 10,
	top = 14,
	bottom = 0,
}

-- Add key bindings to the config table
config.keys = {
	{
		key = "s",
		mods = "CMD",
		action = wezterm.action.SendKey({
			key = "s",
			mods = "CTRL",
		}),
	},
}

return config
