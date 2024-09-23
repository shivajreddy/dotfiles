local wezterm = require("wezterm")

local M = {}

function M.apply(c)
	c.colors = {
		background = "#09080C",
		cursor_fg = "#191724",
		cursor_bg = "#ebbcba",

		-- tab_bar = {
		-- 	-- The color of the inactive tab bar edge/divider
		-- 	inactive_tab_edge = "#575757",
		-- },

		tab_bar = {
			-- The color of the strip that goes along the top of the window
			-- (does not apply when fancy tab bar is in use)
			background = "#121018",

			-- The active tab is the one that has focus in the window
			active_tab = {
				-- The color of the background area for the tab
				bg_color = "#09080C",
				fg_color = "#c0c0c0",

				-- Specify whether you want "Half", "Normal" or "Bold" intensity for the
				-- label shown for this tab.
				-- The default is "Normal"
				intensity = "Normal",

				-- Specify whether you want "None", "Single" or "Double" underline for
				-- label shown for this tab.
				-- The default is "None"
				underline = "None",

				-- Specify whether you want the text to be italic (true) or not (false)
				-- for this tab.  The default is false.
				italic = false,

				-- Specify whether you want the text to be rendered with strikethrough (true)
				-- or not for this tab.  The default is false.
				strikethrough = false,
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

return M
