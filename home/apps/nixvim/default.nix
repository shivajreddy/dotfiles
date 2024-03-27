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
			};



		};

	};



}

