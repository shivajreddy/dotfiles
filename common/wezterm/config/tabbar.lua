local wezterm = require("wezterm")

local M = {}

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
		background = "#111019",
		cursor_fg = "#191724",
		cursor_bg = "#ebbcba",
	}

	c.colors.tab_bar = {
		-- background = "#1f1d2e",
		background = "rgba(0,0,0,0)",

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

wezterm.on("update-status", function(window, _)
	local SOLID_LEFT_ARROW = wezterm.nerdfonts.pl_right_hard_divider
	local segments = segments_for_right_status(window)

	local color_scheme = window:effective_config().resolved_palette
	-- Note the use of wezterm.color.parse here, this returns
	-- a Color object, which comes with functionality for lightening
	-- or darkening the colour (amongst other things).
	local bg = wezterm.color.parse(color_scheme.background)
	local fg = color_scheme.foreground

	-- Each powerline segment is going to be coloured progressively
	-- darker/lighter depending on whether we're on a dark/light colour
	-- scheme. Let's establish the "from" and "to" bounds of our gradient.
	local gradient_to, gradient_from = bg, bg
	if appearance.is_dark() then
		gradient_from = gradient_to:lighten(0.2)
	else
		gradient_from = gradient_to:darken(0.2)
	end

	-- Yes, WezTerm supports creating gradients, because why not?! Although
	-- they'd usually be used for setting high fidelity gradients on your terminal's
	-- background, we'll use them here to give us a sample of the powerline segment
	-- colours we need.
	local gradient = wezterm.color.gradient(
		{
			orientation = "Horizontal",
			colors = { gradient_from, gradient_to },
		},
		#segments -- only gives us as many colours as we have segments.
	)

	-- We'll build up the elements to send to wezterm.format in this table.
	local elements = {}

	for i, seg in ipairs(segments) do
		local is_first = i == 1

		if is_first then
			table.insert(elements, { Background = { Color = "none" } })
		end
		table.insert(elements, { Foreground = { Color = gradient[i] } })
		table.insert(elements, { Text = SOLID_LEFT_ARROW })

		table.insert(elements, { Foreground = { Color = fg } })
		table.insert(elements, { Background = { Color = gradient[i] } })
		table.insert(elements, { Text = " " .. seg .. " " })
	end

	window:set_right_status(wezterm.format(elements))
end)

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
