-- local mocha = require("catppuccin.palettes").get_palette("mocha")

local bufferline = require("bufferline")

return {
	"akinsho/bufferline.nvim",
	event = "VeryLazy",
	opts = {
		options = {
			always_show_bufferline = true,
			style_preset = bufferline.style_preset.no_italic,
			show_buffer_close_icons = false,
		},

		--[[
		highlights = require("catppuccin.groups.integrations.bufferline").get({
			styles = { "italic", "bold" },
			custom = {
				all = {
					fill = { bg = "#000000" },
				},
				mocha = {
					-- buffer_visible = { bg = "#b4befe", fg = "#11111b" },
					buffer_selected = { bg = "#6c7086", fg = "#11111b" },
					-- indicator_selected = { fg = "#89b4fa" },
					-- separators
					separator = { bg = "#6c7086", fg = "#11111b" },
					separator_visible = { bg = "#6c7086", fg = "#11111b" },
					separator_selected = { bg = "#6c7086", fg = "#11111b" },
					offset_separator = { bg = "#6c7086", fg = "#11111b" },
				},
				latte = {
					background = { fg = "#000000" },
				},
			},
		}),
    --]]

		-- Customize Catppuccin colors
		-- https://github.com/catppuccin/nvim?tab=readme-ov-file
		-- after = "catppuccin",
		-- config = function()
		-- 	require("bufferline").setup({
		-- 		highlights = require("catppuccin.groups.integrations.bufferline").get(),
		-- 	})
		-- end,
	},
}
