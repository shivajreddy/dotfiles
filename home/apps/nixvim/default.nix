{ config, lib, pkgs, ... }:


{
	imports = [
		# ./bufferline.nix
	];

	programs.nixvim = {
		enable = true;

		colorschemes.catppuccin.enable = true;

		clipboard.providers.wl-copy.enable = true;

		keymaps = [
		{
			action = "<cmd>w<CR>";
			key = "<Leader-w>";
			options = {noremap = true; silent = true;};
		}
		{
			action = "<cmd>Telescope live_grep<CR>";
			key = "<Leader>sg";
		}
		{
			action = "<cmd>Neotree toggle<CR>";
			key = "<C-n>";
		}
		];


		plugins = {
			lualine.enable = true;
			bufferline.enable = true;
			auto-save = {
				enable = true;
				enableAutoSave = true;

			};

			tmux-navigator.enable = true;

			which-key.enable = true;

			telescope.enable = true;

			treesitter.enable = true;

			luasnip.enable = true;

			neo-tree.enable = true;

			lsp = {
				enable = true;
				servers = {
					pyright.enable = true;
					lua-ls.enable = true;
					rust-analyzer = {
						enable = true;
						installRustc = true;
						installCargo = true;
					};
				};
			};

			cmp = {
				enable = true;
				autoEnableSources = true;
				settings.sources = [
				{name = "nvim_lsp";}
				{name = "buffer";}
				];
				settings.mapping = {
					__raw = ''
						cmp.mapping.preset.insert({
								['<C-b>'] = cmp.mapping.scroll_docs(-4),
								['<C-f>'] = cmp.mapping.scroll_docs(4),
								['<C-Space>'] = cmp.mapping.complete(),
								['<C-e>'] = cmp.mapping.abort(),
								['<CR>'] = cmp.mapping.confirm({ select = true }),
								})
					'';
				};
			};



		};

	};



}

