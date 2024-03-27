{ config, lib, pkgs, ... }:

{
	imports = [
		# ./bufferline.nix
	];

	programs.nixvim = {
		enable = true;

		colorschemes.catppuccin.enable = true;

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

		};

	};



}

