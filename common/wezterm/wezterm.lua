--[[ README

  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::::::::::::::::::::     Author: Shiva        ::::::::::::::::::::
  ::::::::::::::::::::   Date: 10-22-2024       ::::::::::::::::::::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  :: How to set up config is at ::
  https://wezfurlong.org/wezterm/config/files.html#quick-start

  :: Windows ::
  - This is how you create a symlink
  mklink /D "C:\Users\sreddy\.config\wezterm" "\\wsl$\Ubuntu\home\shiva\dotfiles\common\wezterm"
  - so create a symlink to this `wezterm` director from the %HOME%/.config folder of windows-OS,
  because thats where wezterm will look for config

  :: Mac & Linux ::
  - set env variables as usual

--]]

local wezterm = require("wezterm")

-- Load keymaps, utils, tabbar from config folder
local mac_keys = require("config.mac_keys")
local win_keys = require("config.win_keys")
local utils = require("config.utils")
local tabbar_settings = require("config.tabbar")

local c = wezterm.config_builder()

tabbar_settings.apply(c)

-- :::::::::::    GENERAL    :::::::::::
c.automatically_reload_config = true
c.window_close_confirmation = "NeverPrompt"
c.window_padding = { left = 10, right = 10, top = 20, bottom = 0 }
c.adjust_window_size_when_changing_font_size = false
c.audible_bell = "Disabled"

-- :::::::::::    THEME    :::::::::::
c.color_scheme = "catppuccin-mocha"

-- :::::::::::    FONT    :::::::::::
c.font = wezterm.font_with_fallback({
	"Berkeley Mono",
	-- "JetBrains Mono",
	-- "IBM Plex Mono",
	-- "Iosevka",
	-- "Iosevka Fixed",
	"Symbols Nerd Font",
})
c.harfbuzz_features = { "calt=0", "clig=0", "liga=0", "ss01=0" }

-- :::::::::::    MacOS Setttings    :::::::::::
if utils.is_darwin() then
	-- Set keymappings based on os
	mac_keys.apply(c)
	c.font_size = 19
	c.window_frame.font_size = 14 -- tab bar font size
	c.macos_window_background_blur = 80

	c.front_end = "OpenGL"
	-- c.front_end = "WebGpu"
end

-- :::::::::::    WINDOWS  Setttings    :::::::::::
if utils.is_windows() then
	-- ::::::  Startup stuff  ::::::
	c.wsl_domains = {
		{
			name = "WSL:Ubuntu",
			distribution = "Ubuntu",
			default_cwd = "/home//shiva",
		},
	}
	c.default_domain = "WSL:Ubuntu"

	--Set default_prog only if the OS is Windows
	-- c.default_prog = { "wsl", "--cd", "/home/shiva" }

	-- c.front_end = "WebGpu"
	c.front_end = "OpenGL"

	win_keys.apply(c)
	c.font_size = 14
	c.window_frame.font_size = 10 -- tab bar font size

	-- c.window_background_opacity = 0.91 -- This is good for focus
	-- c.window_background_opacity = 0.85
	c.window_background_opacity = 1
	-- c.text_background_opacity = 0.3

	c.window_decorations = "RESIZE"
	c.win32_system_backdrop = "Acrylic" -- Acrylic  Auto Mica Tabbed
	-- c.max_fps = 144
	-- c.animation_fps = 60
	-- c.cursor_blink_rate = 250

	wezterm.on("format-window-title", function()
		return ""
	end)
end

-- :::::::::::    WINDOW-TITLE    :::::::::::
wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
	-- print("🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥")
	-- print("🔥window", window)
	-- print("🔥pane", pane)
	-- print("🔥pane", wezterm.CurrentPaneDomain)
	-- print("🔥tab", tab)
	-- print("🔥tab.tab_title", tab.tab_title)
	-- print("🔥tab.active_pane.title", tab.active_pane.title)
	-- print("🔥tabs", tabs)
	-- print("🔥panes", panes)
	-- print("🔥config", config)
	-- print("🔥hover", hover)
	-- print("🔥max_width", max_width)
	-- print("🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥")
	local title = utils.tab_title(tab)
	if tab.is_active then
		return {
			{ Text = " " .. title .. " " },
		}
	end
	return " " .. title .. " "
end)

-- Session Management
local session_manager = require("wezterm-session-manager/session-manager")
wezterm.on("save_session", function(window)
	session_manager.save_state(window)
end)
wezterm.on("load_session", function(window)
	session_manager.load_state(window)
end)
wezterm.on("restore_session", function(window)
	session_manager.restore_state(window)
end)

return c
