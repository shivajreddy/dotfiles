return {
	-- NOTE 5: set up the language's lsp config here
	{
		"neovim/nvim-lspconfig",
		---@class PluginLspOpts
		opts = {
			inlay_hints = {},
			-- inlay_hints = { enabled = true }, -- this is deprecated, use this instead? inlay_hint.enable(),
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

				-- Nix Lsp
				nil_ls = {},
			},
		},
	},
}

--[[
08:34:05 PM msg_show "~/projects/MyRust/borrowchecker/src/generics.rs" 
08:34:05 PM msg_show "~/projects/MyRust/borrowchecker/src/generics.rs" 46L, 654B
   Warn  08:34:07 PM notify.warn vim.lsp.inlay_hint.enable(bufnr:number, enable:boolean) is deprecated, 
 use vim.lsp.inlay_hint.enable(enable:boolean, filter:table) instead. :help deprecated
Feature was removed in Nvim 0.10-dev
   Warn  08:34:07 PM notify.warn stack traceback:
	...ed-52d2851/share/nvim/runtime/lua/vim/lsp/inlay_hint.lua:382: in function 'enable'
	...zpn68qr-lazy-plugins/LazyVim/lua/lazyvim/util/toggle.lua:70: in function 'inlay_hints'
	...68qr-lazy-plugins/LazyVim/lua/lazyvim/config/keymaps.lua:122: in function <...68qr-lazy-plugins/LazyVim/lua/lazyvim/config/keymaps.lua:122>
   Error  08:34:07 PM msg_show.emsg E5108: Error executing lua: ...ed-52d2851/share/nvim/runtime/lua/vim/lsp/inlay_hint.lua:387: see :help vim.lsp.inlay_hint.enable() for updated parameters
stack traceback:
	[C]: in function 'error'
	...ed-52d2851/share/nvim/runtime/lua/vim/lsp/inlay_hint.lua:387: in function 'enable'
	...zpn68qr-lazy-plugins/LazyVim/lua/lazyvim/util/toggle.lua:70: in function 'inlay_hints'
	...68qr-lazy-plugins/LazyVim/lua/lazyvim/config/keymaps.lua:122: in function <...68qr-lazy-plugins/LazyVim/lua/lazyvim/config/keymaps.lua:122>
--]]
