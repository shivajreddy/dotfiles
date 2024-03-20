{ config, lib, pkgs, ... }:


/*
basically for plugin configation, 
1. you can simply import the file using builtin.readFile
2. or do nix way, define a set with two keys -> `plugin` and `config`
  config accepts string, but it should be a lua command, but for readablity
  make a function at the top that does this convertion
*/
let
    toLua = str: "lua << EOF\n${str}\nEOF\n";
    toLuaFile = file: "lua << EOF\n${builtins.readFile file}\nEOF\n";
in
{

	programs.neovim = {
		enable = true;


		extraLuaConfig = ''
		${builtins.readFile ./options.lua}
		'';

		plugins = with pkgs.vimPlugins; [

                        {
                          plugin = comment-nvim;
                          config = toLua("require(\"Comment\").setup()");
                        }

                        {
                          plugin = nvim-lspconfig;
                          config = toLuaFile ./plugin/lsp.lua;
                        }

                        {
                          plugin = catppuccin;
                          config = "colorscheme catppuccin";
                        }

			nvim-cmp
			
			cmp_luasnip
			cmp-nvim-lsp

			lualine-nvim
			nvim-web-devicons

			(nvim-treesitter.withPlugins (p: [
				p.tree-sitter-nix
				p.tree-sitter-vim
				p.tree-sitter-bash
				p.tree-sitter-lua
				p.tree-sitter-python
				p.tree-sitter-json
				p.tree-sitter-rust
			]))

			vim-nix

		];

	};
}

