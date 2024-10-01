local wezterm = require("wezterm")

local M = {}

function M.apply(c)
	c.colors = {
		-- background = "#09080C",
		-- background = "#191724",
		background = "#111019",
		cursor_fg = "#191724",
		cursor_bg = "#ebbcba",

		-- tab_bar = {
		-- 	-- The color of the inactive tab bar edge/divider
		-- 	inactive_tab_edge = "#575757",
		-- },

		tab_bar = {
			-- The color of the strip that goes along the top of the window
			-- (does not apply when fancy tab bar is in use)
			-- background = "#1f1d2e",
			background = "#000000",
			-- background = "#121018",

			-- The active tab is the one that has focus in the window
			active_tab = {
				-- bg_color = "#09080C",
				-- bg_color = "#11111b",
				bg_color = "#1f1d2e",
				-- fg_color = "#c0c0c0",
				fg_color = "#ebbcba",

				-- intensity = "Normal",
				intensity = "Bold",
				underline = "None",
				strikethrough = false,
				italic = false,
			},

			-- Inactive tabs are the tabs that do not have focus
			inactive_tab = {
				bg_color = "#1C1825",
				fg_color = "#808080",

				-- The same options that were listed under the `active_tab` section above
				-- can also be used for `inactive_tab`.
			},

			-- You can configure some alternate styling when the mouse pointer
			-- moves over inactive tabs
			inactive_tab_hover = {
				bg_color = "#1C1825",
				fg_color = "#808080",
				italic = false,
			},

			-- The new tab button that let you create new tabs
			new_tab = {
				bg_color = "#09080C",
				fg_color = "#c0c0c0",

				-- The same options that were listed under the `active_tab` section above
				-- can also be used for `new_tab`.
			},

			-- You can configure some alternate styling when the mouse pointer
			-- moves over the new tab button
			new_tab_hover = {
				bg_color = "#09080C",
				fg_color = "#c0c0c0",
				italic = false,
			},
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
