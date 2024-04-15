return {

	-- NOTE 5: set up the language's lsp config here
	{
		"neovim/nvim-lspconfig",
		---@class PluginLspOpts
		opts = {
			inlay_hints = { enabled = true },
			servers = {
				-- pyright will be automatically installed with mason and loaded with lspconfig
				pyright = {},
				clangd = {},
				cmake = {},
				rust_analyzer = {
					tools = {},
					inlay_hints = {
						enabled = true,
						auto = true,
						show_parameter_hints = true,
					},
				},
				gopls = {},
			},
		},
	},
}
