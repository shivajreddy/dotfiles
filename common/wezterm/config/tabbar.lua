local wezterm = require("wezterm")

local M = {}

function M.apply(c)
	c.hide_tab_bar_if_only_one_tab = false
	c.tab_bar_at_bottom = false
	c.use_fancy_tab_bar = true
	c.tab_and_split_indices_are_zero_based = true
	-- c.tab_max_width = 16

	c.window_frame = {
		font = require("wezterm").font("Berkeley Mono"),
		font_size = 11.0,
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
		background = "#111019",
		cursor_fg = "#191724",
		cursor_bg = "#ebbcba",
	}

	c.colors.tab_bar = {
		-- background = "#1f1d2e",
		background = "rgba(0,0,0,0)",

		active_tab = {
			bg_color = "#1C1825",
			fg_color = "#ebbcba",

			intensity = "Bold",
			underline = "None",
			strikethrough = false,
			italic = false,
		},

		inactive_tab = {
			-- bg_color = "#1f1d2e",
			bg_color = "rgba(0,0,0,0)",
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

wezterm.on("update-right-status", function(window, pane)
	-- Each element holds the text for a cell in a "powerline" style << fade
	local cells = {}

	-- Figure out the cwd and host of the current pane.
	local cwd_uri = pane:get_current_working_dir()
	if cwd_uri then
		local cwd = ""
		local hostname = ""
		local home = wezterm.home_dir

		if type(cwd_uri) == "userdata" then
			-- Running on a newer version of wezterm and we have
			-- a URL object here, making this simple!
			cwd = cwd_uri.file_path
			hostname = cwd_uri.host or wezterm.hostname()
		else
			-- Older version of wezterm
			cwd_uri = cwd_uri:sub(8)
			local slash = cwd_uri:find("/")
			if slash then
				hostname = cwd_uri:sub(1, slash - 1)
				cwd = cwd_uri:sub(slash):gsub("%%(%x%x)", function(hex)
					return string.char(tonumber(hex, 16))
				end)
			end
		end

		-- Remove the domain name portion of the hostname
		local dot = hostname:find("[.]")
		if dot then
			hostname = hostname:sub(1, dot - 1)
		end
		if hostname == "" then
			hostname = wezterm.hostname()
		end

		-- Replace the home directory path with ~
		if cwd:sub(1, 11) == "/home/shiva" then
			cwd = "~" .. cwd:sub(12)
		end

		table.insert(cells, cwd)
		table.insert(cells, hostname)
	end

	-- Date/Time in the style: "Wed Mar 3 08:14"
	local date = wezterm.strftime("%a %b %-d %H:%M  ")
	table.insert(cells, { text = date, is_date = true })

	-- Add battery info if available
	for _, b in ipairs(wezterm.battery_info()) do
		table.insert(cells, string.format("%.0f%%", b.state_of_charge * 100))
	end

	-- Powerline symbols
	local LEFT_ARROW = utf8.char(0xe0b3)
	local SOLID_LEFT_ARROW = utf8.char(0xe0b2)

	-- Color palette for the backgrounds of each cell
	local colors = {
		"#101019",
		"#191724",
		"#1f1d2e",
		"#3c1361",
		"#52307c",
		"#663a82",
		"#7c5295",
		"#b491c8",
	}

	-- Foreground colors
	local text_fg = "#c0c0c0"
	local date_fg = "#ebbcba"

	-- Elements to be formatted
	local elements = {}
	local num_cells = 0

	-- Translate a cell into elements
	function push(cell, is_last)
		local cell_no = num_cells + 1
		local cell_text = cell.text or cell
		local fg_color = cell.is_date and date_fg or text_fg

		if cell_no == 1 then
			table.insert(elements, { Foreground = { Color = colors[cell_no] } })
			table.insert(elements, { Text = SOLID_LEFT_ARROW })
		end
		table.insert(elements, { Foreground = { Color = fg_color } })
		table.insert(elements, { Background = { Color = colors[cell_no] } })
		table.insert(elements, { Text = " " .. cell_text .. " " })
		if not is_last then
			table.insert(elements, { Foreground = { Color = colors[cell_no + 1] } })
			table.insert(elements, { Text = SOLID_LEFT_ARROW })
		end
		num_cells = num_cells + 1
	end

	-- Process cells
	while #cells > 0 do
		local cell = table.remove(cells, 1)
		push(cell, #cells == 0)
	end

	-- Set the right status with the formatted elements
	window:set_right_status(wezterm.format(elements))
end)

return M
