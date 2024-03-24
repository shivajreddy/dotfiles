require("neo-tree").setup({
	branch = "v3.x",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
		"MunifTanjim/nui.nvim",
		-- "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
	},
	mappings = {

	}
})

vim.keymap.set('n', '<C-n>', '<Cmd>Neotree toggle<CR>')
vim.keymap.set('n', '<Leader-e>', '<Cmd>Neotree toggle<CR>')

