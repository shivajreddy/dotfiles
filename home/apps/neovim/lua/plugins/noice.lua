return {
	"folke/noice.nvim",
	opts = {

		lsp = {
			override = {
				["vim.lsp.util.convert_input_to_markdown_lines"] = true,
				["vim.lsp.util.stylize_markdown"] = true,
				["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
			},
		},

		presets = { bottom_search = true, command_palette = false, lsp_doc_border = true },
	},
}
