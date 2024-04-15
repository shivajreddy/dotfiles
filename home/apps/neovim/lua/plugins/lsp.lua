return {

	-- NOTE 5: set up the language's lsp config here
	{
		"neovim/nvim-lspconfig",
		---@class PluginLspOpts
		opts = {
			servers = {
				-- pyright will be automatically installed with mason and loaded with lspconfig
				pyright = {},
				clangd = {},
				cmake = {},
				rust_analyzer = {},
				gopls = {},
			},
		},
		inlay_hints = { enabled = true },
	},
}
