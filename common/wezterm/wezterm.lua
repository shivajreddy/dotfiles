--[[

  Wezterm config by shiva

    how to set up config is at:
    https://wezfurlong.org/wezterm/config/files.html#quick-start

    -- Windows

    This is how you create a symlink
    mklink /D "C:\Users\sreddy\.config\wezterm" "\\wsl$\Ubuntu\home\shiva\dotfiles\common\wezterm"

    so create a symlink to this `wezterm` director from the %HOME%/.config folder of windows-OS,
    because thats where wezterm will look for config

    -- Mac & Linux
    set env variables as usual

--]]

local wezterm = require("wezterm")

local c = wezterm.config_builder()

local mac_keys = require("config.mac_keys")
local win_keys = require("config.win_keys")
local tabbar_settings = require("config.tabbar")

-- set the theme settings from config/tabbar.lua
tabbar_settings.apply(c)

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

-- General
c.automatically_reload_config = true
c.window_close_confirmation = "NeverPrompt"

-- Theme
local opacity = 0.75
local transparent_bg = "rgba(22, 24, 26, " .. opacity .. ")"
c.color_scheme = "catppuccin-mocha"

-- c.front_end = "OpenGL"
c.front_end = "WebGpu"

-- c.text_background_opacity = 0.1
c.window_background_opacity = 0.8
-- c.win32_system_backdrop = "Acrylic"
-- c.win32_system_backdrop = "Auto"
-- c.win32_system_backdrop = "Mica"
-- c.win32_system_backdrop = "Tabbed"
-- c.macos_window_background_blur = 80
c.window_decorations = "RESIZE"
c.window_background_opacity = opacity
c.window_close_confirmation = "NeverPrompt"
c.win32_system_backdrop = "Acrylic"
c.max_fps = 144
c.animation_fps = 60
c.cursor_blink_rate = 250

--[[ Set the wsl as the domain when using windows
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

--[[ Set default_prog only if the OS is Windows
--]]
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
	"Berkeley Mono",
	"JetBrains Mono",
	-- "IBM Plex Mono",
	"Symbols Nerd Font",
})
c.harfbuzz_features = { "calt=0", "clig=0", "liga=0", "ss01=0" }

require("wezterm").on("format-window-title", function()
	return ""
end)
-- require("wezterm").on("format-tab-title", function()
-- 	return ""
-- end)

c.window_padding = { left = 10, right = 10, top = 20, bottom = 0 }
c.adjust_window_size_when_changing_font_size = false
c.audible_bell = "Disabled"
-- c.inactive_pane_hsb = { brightness = 0.6, saturation = 0.1 }

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
