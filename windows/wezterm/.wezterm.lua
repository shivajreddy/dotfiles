local wezterm = require("wezterm")
local c = wezterm.config_builder()

c.font = wezterm.font_with_fallback({
	"Berkeley Mono",
	-- "Cascadia Code",
	-- "IBM Plex Mono",
	-- "Comic Code Ligatures",
	"Symbols Nerd Font",
})
c.front_end = "WebGpu"
c.font_size = 12
c.harfbuzz_features = { "calt=1", "ss01=1" }
c.command_palette_font_size = c.font_size * 1.1
c.window_frame = { font = wezterm.font("IBM Plex Sans") }

-- c.window_background_opacity = 0.96
-- c.macos_window_background_blur = 20

-- Startup Program
c.default_prog = { "wsl", "--cd", "/home/shiva" }
c.hide_tab_bar_if_only_one_tab = true

c.window_decorations = "TITLE | RESIZE"
require("wezterm").on("format-window-title", function()
	return ""
end)
require("wezterm").on("format-tab-title", function()
	return ""
end)

c.window_padding = { left = 0, right = 0, top = 50, bottom = 0 }
c.adjust_window_size_when_changing_font_size = false
c.audible_bell = "Disabled"
c.inactive_pane_hsb = { brightness = 0.90 }

-- Theme
c.color_scheme = "catppuccin-mocha"
c.colors = {
	background = "#111019",
	-- cursor_fg = "#e0def4",
	-- cursor_bg = "#524f67",
	cursor_fg = "#191724",
	cursor_bg = "#ebbcba",
}

c.window_padding = {
	left = 10,
	right = 10,
	top = 14,
	bottom = 0,
}

-- Keybindings
c.keys = {
	{
		key = "s",
		mods = "CMD",
		action = wezterm.action.SendKey({
			key = "s",
			mods = "CTRL",
		}),
	},
}

return c
