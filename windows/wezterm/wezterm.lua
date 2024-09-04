local wezterm = require("wezterm")
local c = wezterm.config_builder()

require("config.keys").apply(c)

c.font = wezterm.font_with_fallback({
	"Berkeley Mono",
	-- "Cascadia Code",
	-- "IBM Plex Mono",
	-- "Comic Code Ligatures",
	"Symbols Nerd Font",
})
-- c.front_end = "WebGpu"
c.font_size = 12
c.harfbuzz_features = { "calt=1", "ss01=1" }
c.command_palette_font_size = c.font_size * 1.1

-- c.window_background_opacity = 0.95
-- c.macos_window_background_blur = 20

c.default_prog = { "wsl", "--cd", "/home/shiva" }

-- https://wezfurlong.org/wezterm/config/lua/config/window_decorations.html?h=window_de
-- c.window_decorations = "NONE"
-- c.window_decorations = "TITLE|RESIZE"
-- c.window_decorations = "TITLE|RESIZE|INTEGRATED_BUTTONS"
c.window_decorations = "RESIZE"
c.window_frame = {
	inactive_titlebar_bg = "#353535",
	active_titlebar_bg = "#2b2042",
	inactive_titlebar_fg = "#cccccc",
	active_titlebar_fg = "#ffffff",
	inactive_titlebar_border_bottom = "#2b2042",
	active_titlebar_border_bottom = "#2b2042",
	button_fg = "#cccccc",
	button_bg = "#2b2042",
	button_hover_fg = "#ffffff",
	button_hover_bg = "#3b3052",
}
c.window_frame = { font = wezterm.font("IBM Plex Sans"), font_size = 10 }

c.window_padding = { left = 0, right = 0, top = 20, bottom = 00 }
c.adjust_window_size_when_changing_font_size = false
c.audible_bell = "Disabled"
c.inactive_pane_hsb = { brightness = 0.40 }

-- Theme
c.color_scheme = "catppuccin-mocha"
c.colors = {
	-- background = "#111019",
	-- background = "#101018",
	-- background = "#0D0D13",
	background = "#08080C",
	cursor_fg = "#191724",
	cursor_bg = "#ebbcba",
}

-- c.hide_tab_bar_if_only_one_tab = false
c.tab_bar_at_bottom = true
-- c.use_fancy_tab_bar = true
c.use_fancy_tab_bar = false
c.tab_and_split_indices_are_zero_based = true

return c
