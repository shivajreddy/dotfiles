--[[
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::::::::::::::::::::     Author: Shiva        ::::::::::::::::::::
  ::::::::::::::::::::   Date: 11-20-2024       ::::::::::::::::::::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
--]]

local wezterm = require("wezterm")
-- Load keymaps, utils, tabbar from config folder

local mac_keys = require("config.mac_keys")
local win_keys = require("config.win_keys")
local utils = require("config.utils")
local ui_settings = require("config.ui")

local c = wezterm.config_builder()

ui_settings.apply(c)

-- :::::::::::    GENERAL    :::::::::::
c.automatically_reload_config = true
c.window_close_confirmation = "NeverPrompt"
c.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }
c.adjust_window_size_when_changing_font_size = false
c.audible_bell = "Disabled"
c.selection_word_boundary = "{}[]()\"'`.,;:"

-- :::::::::::    THEME    :::::::::::
c.color_scheme = "catppuccin-mocha"

-- c.front_end = "OpenGL"

-- :::::::::::    FONT    :::::::::::
c.font = wezterm.font_with_fallback({
	-- "BlexMono Nerd Font",
	-- { family="Iosevka Nerd Font" },
	{ family = "Iosevka", weight = "Regular" },
	{ family = "Iosevka", stretch = "Expanded", weight = "Regular" },
	-- "JetBrainsMono Nerd Font",
	"Berkeley Mono",
	"Symbols Nerd Font",
})
-- Disable font ligatures => == <=
c.harfbuzz_features = { "calt=0", "clig=1", "liga=1", "ss01=0" }

-- :::::::::::    MacOS Setttings    :::::::::::
if utils.is_darwin() then
	-- Set keymappings based on os
	mac_keys.apply(c)
	c.font_size = 19
	c.window_frame.font_size = 14 -- tab bar font size
	c.macos_window_background_blur = 80

	-- c.window_decorations = "RESIZE"
end

-- :::::::::::    Linux Setttings    :::::::::::
if utils.is_linux() then
	win_keys.apply(c)
	c.font_size = 18
	c.window_frame.font_size = 10 -- tab bar font size

	-- c.window_background_opacity = 0.91 -- This is good for focus
	-- c.window_background_opacity = 0.85
	c.window_background_opacity = 1
	-- c.text_background_opacity = 0.3

	c.window_decorations = "NONE"
	-- c.max_fps = 144
	-- c.animation_fps = 60
	-- c.cursor_blink_rate = 250

	wezterm.on("format-window-title", function()
		return ""
	end)
end

-- :::::::::::    WINDOWS  Setttings    :::::::::::
if utils.is_windows() then
	-- ::::::  Startup stuff  ::::::

	-- Ubuntu
	c.wsl_domains = {
		{
			name = "WSL:Ubuntu",
			distribution = "Ubuntu",
			default_cwd = "/home//shiva",
		},
	}
	c.default_domain = "WSL:Ubuntu"
	--]]

	--[[ DEBIAN
	c.wsl_domains = {
		{
			name = "WSL:Debian",
			distribution = "Debian",
			default_cwd = "/home//shiva",
		},
	}
	c.default_domain = "WSL:Debian"
	--]]

	--Set default_prog only if the OS is Windows
	-- c.default_prog = { "wsl", "--cd", "/home/shiva" }

	win_keys.apply(c)
	c.font_size = 14
	c.window_frame.font_size = 10 -- tab bar font size

	c.win32_system_backdrop = "Auto" -- Acrylic, Auto, Mica, Tabbed

	-- c.window_background_opacity = 0
	-- c.window_background_opacity = 0.5
	c.window_background_opacity = 0.90 -- This is good for focus
	-- c.window_background_opacity = 1

	-- c.text_background_opacity = 0.3

	c.window_decorations = "RESIZE"
	-- c.max_fps = 144
	-- c.animation_fps = 60
	-- c.cursor_blink_rate = 250

	-- wezterm.on("format-window-title", function()
	-- 	return ""
	-- end)
end

-- :::::::::::    WINDOW-TITLE    :::::::::::
--[[
wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
	local title = utils.tab_title(tab)
	if tab.is_active then
		return {
			{ Text = " " .. title .. " " },
		}
	end
	return " " .. title .. " "
end)
--]]

-- tmux status
wezterm.on("update-right-status", function(window, _)
	local SOLID_LEFT_ARROW = ""
	local ARROW_FOREGROUND = { Foreground = { Color = "#c6a0f6" } }
	local prefix = ""

	if window:leader_is_active() then
		prefix = " " .. utf8.char(0x1f30a) -- ocean wave
		SOLID_LEFT_ARROW = utf8.char(0xe0b2)
	end

	if window:active_tab():tab_id() ~= 0 then
		ARROW_FOREGROUND = { Foreground = { Color = "#1e2030" } }
	end -- arrow color based on if tab is first pane

	window:set_left_status(wezterm.format({
		{ Background = { Color = "#b7bdf8" } },
		{ Text = prefix },
		ARROW_FOREGROUND,
		{ Text = SOLID_LEFT_ARROW },
	}))
end)

-- Smart Workspace switcher https://github.com/MLFlexer/smart_workspace_switcher.wezterm
local workspace_switcher = wezterm.plugin.require("https://github.com/MLFlexer/smart_workspace_switcher.wezterm")
workspace_switcher.apply_to_config(c)
c.default_workspace = "~"
-- Resurrect
-- local resurrect = wezterm.plugin.require("https://github.com/MLFlexer/resurrect.wezterm")

return c
