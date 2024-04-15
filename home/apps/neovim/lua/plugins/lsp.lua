local function set_cmn_lsp_keybinds()
	local lsp_keybinds = {
		{
			key = "K",
			action = vim.lsp.buf.hover,
			options = {
				buffer = 0,
				desc = "hover [K]noledge with LSP",
			},
		},
		{
			key = "gd",
			action = vim.lsp.buf.definition,
			options = {
				buffer = 0,
				desc = "[g]o to [d]efinition with LSP",
			},
		},
		{
			key = "gy",
			action = vim.lsp.buf.type_definition,
			options = {
				buffer = 0,
				desc = "[g]o to t[y]pe definition with LSP",
			},
		},
		{
			key = "gi",
			action = vim.lsp.buf.implementation,
			options = {
				buffer = 0,
				desc = "[g]o to [i]mplementation with LSP",
			},
		},
		{
			key = "<leader>dj",
			action = vim.diagnostic.goto_next,
			options = {
				buffer = 0,
				desc = "Go to next [d]iagnostic with LSP",
			},
		},
		{
			key = "<leader>dk",
			action = vim.diagnostic.goto_prev,
			options = {
				buffer = 0,
				desc = "Go to previous [d]iagnostic with LSP",
			},
		},
		{
			key = "<leader>r",
			action = vim.lsp.buf.rename,
			options = {
				buffer = 0,
				desc = "[r]ename variable with LSP",
			},
		},
	}
	for _, bind in ipairs(lsp_keybinds) do
		vim.keymap.set("n", bind.key, bind.action, bind.options)
	end
end

-- Additional lsp-config
local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())
capabilities.textDocument.completion.completionItem.snippetSupport = true

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
				rust_analyzer = {
					inlayHints = {
						enable = true,
					},
				},
				gopls = {},
			},
			setup = {
				rust_analyzer = function(_, opts)
					require("rust_analyzer").setup({ server = opts })
					return true
				end,
			},

			inlay_hints = { enabled = true },
		},
	},
}
