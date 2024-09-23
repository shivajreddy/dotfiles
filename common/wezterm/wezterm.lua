--[[

  Wezterm config by shiva

    how to set up config is at:
    https://wezfurlong.org/wezterm/config/files.html#quick-start

    -- Windows
    Open the Start menu, search for "Environment Variables," and select
    Edit the system environment variables.

    In the System Properties window, click the Environment Variables button.

    Under User variables (or System variables, depending on your preference),
    click New and create one of the following variables:

    For specifying a configuration file:
    Variable name: XDG_CONFIG_HOME
    Variable value: <path of the folder that has wezterm/wezterm.lua>
    for tars the value is: \\wsl.localhost\Ubuntu\home\shiva\dotfiles\common\

    - the documentation says we can set either WEZTERM_CONFIG_FILE or 
      XDG_CONFIG_HOME, i chose later cuz we use relative paths for importing
      keys and utils lua files, and if we used WEZTERM_CONFIG_FILE wezterm will
      check for keys.lua file in windows folder but not relatively

    -- Mac & Linux
    set env variables as usual

--]]

local wezterm = require("wezterm")

local c = wezterm.config_builder()

local mac_keys = require("config.mac_keys")
local win_keys = require("config.win_keys")
local theme_settings = require("config.theme")

-- set the theme settings from config/theme.lua
theme_settings.apply(c)

local utils = require("config.utils")

-- Set keymappings based on os
if utils.is_darwin() then
	mac_keys.apply(c)
	c.font_size = 18
end
if utils.is_windows() then
	win_keys.apply(c)
	c.font_size = 12
end

--[[
-- Set the wsl as the domain when using windows
-- This table will hold the configuration.
-- https://wezfurlong.org/wezterm/config/lua/WslDomain.html
c.wsl_domains = {
	{
		name = "WSL:Ubuntu",
		distribution = "Ubuntu",
		default_cwd = "~",
	},
}
c.default_domain = "WSL:Ubuntu"
--]]

-- Set default_prog only if the OS is Windows
if utils.is_windows() then
	c.default_prog = { "wsl", "--cd", "/home/shiva" }
end

-- Get the directory of the wezterm.lua file
-- local config_dir = wezterm.config_dir
-- Use the absolute path to require the module
-- local config_keys = require(config_dir .. "/config.keys")
-- Apply the configuration
-- config_keys.apply(c)

c.font = wezterm.font_with_fallback({
	-- "JetBrains Mono",
	"Berkeley Mono",
	-- "Cascadia Code",
	-- "IBM Plex Mono",
	-- "Comic Code Ligatures",
	"Symbols Nerd Font",
})
c.front_end = "WebGpu"
-- c.harfbuzz_features = { "calt=1", "ss01=1" }

c.window_background_opacity = 0.80
c.macos_window_background_blur = 70

require("wezterm").on("format-window-title", function()
	return ""
end)
-- require("wezterm").on("format-tab-title", function()
-- 	return ""
-- end)

c.hide_tab_bar_if_only_one_tab = false
c.tab_bar_at_bottom = true
c.use_fancy_tab_bar = false
c.tab_and_split_indices_are_zero_based = true

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
-- c.window_frame = { font = wezterm.font("IBM Plex Sans"), font_size = 10 }

c.window_padding = { left = 10, right = 10, top = 20, bottom = 0 }
c.adjust_window_size_when_changing_font_size = false
c.audible_bell = "Disabled"
c.inactive_pane_hsb = { brightness = 0.6, saturation = 0.1 }

-- Theme
c.color_scheme = "catppuccin-mocha"

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
