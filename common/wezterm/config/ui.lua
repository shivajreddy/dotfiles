local wezterm = require("wezterm")
local utils = require("config.utils")

local M = {}

function M.apply(c)
	-- Font configuration
	c.font_size = 14
	c.font = wezterm.font_with_fallback({
		{ family = "Iosevka Nerd Font", weight = "Regular" },
		{ family = "JetBrains Mono", weight = "Bold" },
		{ family = "Noto Sans Mono", weight = "Bold" },
	})

	-- Color scheme base
	c.color_scheme = "Catppuccin Mocha"

	-- Custom colors
	c.colors = {
		background = "#000000",
		cursor_bg = "#DA3B01",
		cursor_fg = "#000000",
		tab_bar = {
			background = "#111111",
			active_tab = {
				bg_color = "#DA3B01",
				fg_color = "#000000",
			},
			inactive_tab = {
				bg_color = "#1b1b1b",
				fg_color = "#888888",
			},
			inactive_tab_hover = {
				bg_color = "#222222",
				fg_color = "#ffffff",
			},
		},
	}

	-- Window appearance
	c.window_background_opacity = 0.50
	c.win32_system_backdrop = "Tabbed"
	c.macos_window_background_blur = 20
	c.window_decorations = "RESIZE"
	c.initial_cols = 120
	c.initial_rows = 28

	-- Window frame (titlebar)
	c.window_frame = {
		font = wezterm.font_with_fallback({
			{ family = "Iosevka Nerd Font", weight = "Regular" },
			{ family = "JetBrains Mono", weight = "Bold" },
			{ family = "Noto Sans Mono", weight = "Bold" },
		}),
		font_size = 11.0,
		active_titlebar_bg = "#000000",
		inactive_titlebar_bg = "#000000",
	}

	-- Tab bar
	c.use_fancy_tab_bar = false
	c.tab_bar_at_bottom = true
	c.show_new_tab_button_in_tab_bar = false
	c.tab_max_width = 32

	-- Pane settings
	c.inactive_pane_hsb = {
		saturation = 1,
		brightness = 1,
	}
end

-- Format tab titles
local TAB_FIXED_WIDTH = 20
local ZOOM_ICON = wezterm.nerdfonts.cod_search .. " "

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
	local title = utils.tab_title(tab)
	title = title:gsub("^Copy mode: ", "")
	local tab_number = tab.tab_index + 1

	-- Show zoom indicator when pane is zoomed
	local is_zoomed = tab.active_pane.is_zoomed
	local prefix = " " .. tab_number .. " " .. (is_zoomed and ZOOM_ICON or "")
	local suffix = " "

	-- Calculate display widths
	local prefix_width = wezterm.column_width(prefix)
	local suffix_width = wezterm.column_width(suffix)
	local title_width = wezterm.column_width(title)
	local total_width = prefix_width + title_width + suffix_width

	-- Truncate title if over fixed width
	if total_width > TAB_FIXED_WIDTH then
		local available = TAB_FIXED_WIDTH - prefix_width - suffix_width - 1 -- 1 for ellipsis
		title = wezterm.truncate_right(title, available) .. "…"
	end

	-- Pad to fixed width
	local content = prefix .. title .. suffix
	local content_width = wezterm.column_width(content)
	if content_width < TAB_FIXED_WIDTH then
		content = content .. string.rep(" ", TAB_FIXED_WIDTH - content_width)
	end

	if tab.is_active then
		return {
			{ Attribute = { Intensity = "Bold" } },
			{ Text = content },
			{ Background = { Color = "#111111" } },
			{ Text = " " },
		}
	else
		return {
			{ Text = content },
			{ Background = { Color = "#111111" } },
			{ Text = " " },
		}
	end
end)

-- Status bar (LEADER and COPY MODE indicators)
wezterm.on("update-right-status", function(window, pane)
	local status = {}

	table.insert(status, { Attribute = { Intensity = "Bold" } })

	local name = window:active_key_table()
	if name then
		local display_name = name:upper():gsub("_", " ")
		table.insert(status, { Foreground = { Color = "#000000" } })
		table.insert(status, { Background = { Color = "#a6e3a1" } })
		table.insert(status, { Text = " " .. display_name .. " " })
	end

	if window:leader_is_active() then
		table.insert(status, { Foreground = { Color = "#000000" } })
		table.insert(status, { Background = { Color = "#f9e2af" } })
		table.insert(status, { Text = " LEADER " })
	end

	window:set_right_status(wezterm.format(status))
end)

return M
