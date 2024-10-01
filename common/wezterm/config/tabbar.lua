local wezterm = require("wezterm")

local M = {}

-- The filled in variant of the < symbol
local SOLID_LEFT_ARROW = wezterm.nerdfonts.pl_right_hard_divider
-- The filled in variant of the > symbol
local SOLID_RIGHT_ARROW = wezterm.nerdfonts.pl_left_hard_divider

function M.apply(c)
	c.hide_tab_bar_if_only_one_tab = false
	c.tab_bar_at_bottom = false
	c.use_fancy_tab_bar = true
	c.tab_and_split_indices_are_zero_based = true

	-- c.tab_max_width = 16

	-- Fancy tab bar
	c.window_frame = {
		font_size = 10.0,
		inactive_titlebar_bg = "#1f1d2e",
		active_titlebar_bg = "#1f1d2e",
		-- inactive_titlebar_bg = "#353535",
		-- active_titlebar_bg = "#2b2042",
		inactive_titlebar_fg = "#cccccc",
		active_titlebar_fg = "#ffffff",
		inactive_titlebar_border_bottom = "#2b2042",
		active_titlebar_border_bottom = "#2b2042",
		button_fg = "#cccccc",
		button_bg = "#2b2042",
		button_hover_fg = "#ffffff",
		button_hover_bg = "#3b3052",
	}

	c.colors = {
		-- background = "#09080C",
		-- background = "#191724",
		background = "#111019",
		cursor_fg = "#191724",
		cursor_bg = "#ebbcba",
	}

	c.colors.tab_bar = {
		-- background = "#1f1d2e",
		background = "#000000",

		active_tab = {
			bg_color = "#1f1d2e",
			fg_color = "#ebbcba",

			intensity = "Bold",
			underline = "None",
			strikethrough = false,
			italic = false,
		},

		inactive_tab = {
			bg_color = "#1C1825",
			fg_color = "#808080",
		},

		inactive_tab_hover = {
			bg_color = "#1C1825",
			fg_color = "#808080",
			italic = false,
		},

		new_tab = {
			bg_color = "#09080C",
			fg_color = "#c0c0c0",
		},

		new_tab_hover = {
			bg_color = "#09080C",
			fg_color = "#c0c0c0",
			italic = false,
		},
	}
end

wezterm.on("update-right-status", function(window, _)
	-- Get the current time in hh:mm AM/PM format and the date in mm-dd-yyyy format
	local time = wezterm.strftime("%I:%M %p") -- 12-hour format with AM/PM
	local date = wezterm.strftime("%m-%d-%Y") -- Date in mm-dd-yyyy format

	-- Set the right status with the formatted time and date
	window:set_right_status(wezterm.format({
		{ Text = time .. "  " .. date },
	}))
end)

return M
