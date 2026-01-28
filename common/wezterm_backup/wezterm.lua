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
c.window_padding = { left = 14, right = 14, top = 14, bottom = 10 }
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

	c.window_background_opacity = 0.90
	c.macos_window_background_blur = 85

	c.window_decorations = "RESIZE"
end

-- :::::::::::    Linux Setttings    :::::::::::
if utils.is_linux() then
	win_keys.apply(c)
	c.font_size = 18
	c.window_frame.font_size = 10 -- tab bar font size

	c.window_background_opacity = 0.95 -- This is good for focus
	-- c.window_background_opacity = 0.85
	-- c.window_background_opacity = 1
	-- c.window_background_opacity = 0.5
	-- c.text_background_opacity = 0.3

	-- c.window_decorations = "NONE"
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

	c.win32_acrylic_accent_color = "#ffffff"
	c.win32_system_backdrop = "Acrylic" -- Acrylic, Auto, Mica, Tabbed

	-- c.window_background_opacity = 0.5
	-- c.window_background_opacity = 0.95 -- This is good for focus
	c.window_background_opacity = 1

	-- c.text_background_opacity = 0.3

	c.window_decorations = "RESIZE"
	-- c.max_fps = 144
	-- c.animation_fps = 60
	-- c.cursor_blink_rate = 250

	-- wezterm.on("format-window-title", function()
	-- 	return ""
	-- end)
end

-- c.show_close_tab_button_in_tabs = false
c.show_new_tab_button_in_tab_bar = false
c.show_close_tab_button_in_tabs = false
-- c.show_tabs_in_tab_bar = false

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
-- The filled in variant of the < symbol
local SOLID_BLANK = wezterm.nerdfonts.md_checkbox_blank
local SOLID_LEFT_ARROW = wezterm.nerdfonts.pl_right_hard_divider
-- The filled in variant of the > symbol
local SOLID_RIGHT_ARROW = wezterm.nerdfonts.pl_left_hard_divider

local SOLID_LEFT_HALF_CIRCLE = wezterm.nerdfonts.ple_left_half_circle_thick
local SOLID_RIGHT_HALF_CIRCLE = wezterm.nerdfonts.ple_right_half_circle_thick

-- This function returns the suggested title for a tab.
-- It prefers the title that was set via `tab:set_title()`
-- or `wezterm cli set-tab-title`, but falls back to the
-- title of the active pane in that tab.
function tab_title(tab_info)
	local title = tab_info.tab_title
	-- if the tab title is explicitly set, take that
	if title and #title > 0 then
		return title
	end
	-- Otherwise, use the title from the active pane
	-- in that tab
	return tab_info.active_pane.title
end

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
	-- local edge_background = "#0b0022"
	local edge_background = "#0e0a01"

	local background = "rgba(0,0,0,0)"
	local foreground = "#808080"

	if tab.is_active then
		background = "#ea9d34"
		foreground = "#0e0a01"
	elseif hover then
		background = "#3b3052"
		foreground = "#909090"
	end

	local edge_foreground = background

	local title = tab_title(tab)

	-- ensure that the titles fit in the available space,
	-- and that we have room for the edges.
	title = wezterm.truncate_right(title, max_width - 2)

	return {
		{ Background = { Color = edge_background } },
		{ Foreground = { Color = edge_foreground } },
		-- { Text = SOLID_BLANK },
		{ Text = SOLID_LEFT_HALF_CIRCLE },
		{ Background = { Color = background } },
		{ Foreground = { Color = foreground } },
		{ Text = title },
		{ Background = { Color = edge_background } },
		{ Foreground = { Color = edge_foreground } },
		-- { Text = SOLID_BLANK },
		{ Text = SOLID_RIGHT_HALF_CIRCLE },
	}
end)

-- tmux status
wezterm.on("update-right-status", function(window, _)
	local LEFT_ARROW = ""
	local ARROW_FOREGROUND = { Foreground = { Color = "#c6a0f6" } }
	local prefix = ""

	if window:leader_is_active() then
		prefix = " " .. utf8.char(0x1f30a) -- ocean wave
		LEFT_ARROW = utf8.char(0xe0b2)
	end

	if window:active_tab():tab_id() ~= 0 then
		ARROW_FOREGROUND = { Foreground = { Color = "#1e2030" } }
	end -- arrow color based on if tab is first pane

	window:set_left_status(wezterm.format({
		{ Background = { Color = "#b7bdf8" } },
		{ Text = prefix },
		ARROW_FOREGROUND,
		{ Text = LEFT_ARROW },
	}))
end)

return c
