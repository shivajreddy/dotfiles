return {
	"akinsho/bufferline.nvim",
	event = "VeryLazy",
	opts = {
		options = {
			show_buffer_close_icons = false,
		},

		highlights = require("catppuccin.groups.integrations.bufferline").get(),

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
