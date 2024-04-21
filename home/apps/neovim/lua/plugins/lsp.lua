return {

	-- NOTE 5: set up the language's lsp config here
	{
		"neovim/nvim-lspconfig",
		---@class PluginLspOpts
		opts = {
			-- inlay_hints = { enabled = true },
			inlay_hint.enable(),
			servers = {
				-- pyright will be automatically installed with mason and loaded with lspconfig
				pyright = {},
				clangd = {},
				cmake = {},
				rust_analyzer = {
					-- this is a good post: https://oneofone.dev/post/neovim-lsp-go-rust/
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

--[[
   Warn  08:25:43 PM notify.warn vim.lsp.inlay_hint.enable(bufnr:number, enable:boolean) is deprecated,
 use vim.lsp.inlay_hint.enable(enable:boolean, filter:table) instead. :help deprecated
--]]
