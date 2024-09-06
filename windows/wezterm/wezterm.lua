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
c.front_end = "WebGpu"
c.font_size = 12
c.harfbuzz_features = { "calt=1", "ss01=1" }

-- c.window_background_opacity = 0.95
-- c.macos_window_background_blur = 20

require("wezterm").on("format-window-title", function()
	return ""
end)
-- require("wezterm").on("format-tab-title", function()
-- 	return ""
-- end)

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

c.window_padding = { left = 10, right = 10, top = 20, bottom = 0 }
c.adjust_window_size_when_changing_font_size = false
c.audible_bell = "Disabled"
c.inactive_pane_hsb = { brightness = 0.6, saturation = 0.1 }

-- Theme
c.color_scheme = "catppuccin-mocha"
c.colors = {
	background = "#09080C",
	cursor_fg = "#191724",
	cursor_bg = "#ebbcba",
}

-- c.hide_tab_bar_if_only_one_tab = false
c.tab_bar_at_bottom = true
-- c.use_fancy_tab_bar = true
c.use_fancy_tab_bar = false
c.tab_and_split_indices_are_zero_based = true

local function get_current_working_dir(tab)
	local current_dir = tab.active_pane.current_working_dir
	local HOME_DIR = string.format("file://%s", os.getenv("HOME"))

	return current_dir == HOME_DIR and "." or string.gsub(current_dir, "(.*[/\\])(.*)", "%2")
end

-- Function to retrieve the tab title or fall back to the active pane's working directory
function tab_title(tab_info)
	local title = tab_info.tab_title
	-- If the tab title is explicitly set, return that
	if title and #title > 0 then
		return title
	end
	-- Otherwise, return the active pane's current working directory
	return "❯ ~ " .. (get_current_working_dir(tab_info))
end

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
	print("result print")
	print(tab_title(tab))
	local title = tab_title(tab)
	if tab.is_active then
		return {
			-- { Background = { Color = "blue" } },
			{ Text = " " .. title .. " " },
		}
	end
	return title
end)

return c
