local mocha = require("catppuccin.palettes").get_palette("mocha")

return {
	"akinsho/bufferline.nvim",
	event = "VeryLazy",
	opts = {
		options = {
			show_buffer_close_icons = false,
		},

		highlights = require("catppuccin.groups.integrations.bufferline").get({
			styles = { "italic", "bold" },
			custom = {
				all = {
					fill = { bg = "#000000" },
				},
				mocha = {
					background = { fg = mocha.text },
				},
				latte = {
					background = { fg = "#000000" },
				},
			},
		}),

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
