{ config, lib, pkgs, ... }:


{
	imports = [
		# ./bufferline.nix
		# ./keymaps.nix
		./completion.nix
	];

	programs.nixvim = {
		enable = true;

		colorschemes.catppuccin.enable = true;

		# clipboard.providers.wl-copy.enable = true;
		clipboard.register = "unnamedplus";

		globals.mapleader = " ";

		# NOTE: hello
		# WTF: test
		# COMMENT: hello
		keymaps = import ./keymaps.nix;
		/*
		[
		{
			action = "<cmd>w<CR>";
			key = "<Leader>w";
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
		*/

		highlight = {
			Note.fg = "#ff00ff";
			Note.bg = "#000000";
			Note.underline = true;
			Note.bold = true;

			Comment.fg = "#ff00ff";
			Comment.bg = "#000000";
			Comment.underline = true;
			Comment.bold = true;

			wtf.fg = "#ff00ff";
			wtf.bg = "#000000";
			wtf.underline = true;
			wtf.bold = true;
		};


		plugins = {

			# Formatting
			conform-nvim = {
				enable = true;
				extraOptions = {
					keys = {
						action = "<cmd>q<CR>";
						key = "<Leader>cF";
						options.desc = "Format Injected Langs";
					};
				};
			};


			lualine.enable = true;
			# bufferline.enable = true;
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

