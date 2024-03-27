{ config, lib, pkgs, ... }:

{
	imports = [
		# ./bufferline.nix
	];

	programs.nixvim = {
		enable = true;


		plugins = {
			lualine.enable = true;

			lsp = {
				enable = true;
				servers = {
					pyright.enable = true;
					lua-ls.enable = true;
					rust-analyzer.enable = true;
				};
			};

		};

	};



}

