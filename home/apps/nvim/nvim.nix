{ config, lib, pkgs, ... }:

{

  # NeoVim Configuration -----------------
  programs.neovim = {
    extraPackages = with pkgs; [
    	# Required for LazyVim
	lua-language-server
	stylua
	# Required for Telescope
	ripgrep
    ];

    plugins = with pkgs.vimPlugins; [
      lazy-nvim
    ];

    extraLuaConfig = 
      let
        # 1. plugins variable 
        plugins = with pkgs.vimPlugins; [
	  # LazyVim plugins
	  LazyVim
	  bufferline-nvim
	  cmp-buffer
	  cmp-nvim-lsp
	  # more plugins here...
	];
	# 2. variable for mk-entry
	mkEntryFromDrv = drv:
	  if lib.isDerivation drv then
	    { name = "${lib.getName drv}"; path = drv; }
	  else
	    drv;
	# 3. variable for lazy path
	lazyPath = pkgs.linkFarm "lazy-plugins" (builtins.map mkEntryFromDrv plugins);
      in
      ''
      '';
    };


    # XDG Configuration -----------------
    # https://github.com/nvim-treesitter/nvim-treesitter#i-get-query-error-invalid-node-type-at-position
    xdg.configFile."nvim/parser".source =
    let
      parsers = pkgs.symlinkJoin {
        name = "treesitter-parsers";
        paths = (pkgs.vimPlugins.nvim-treesitter.withPlugins (plugins: with plugins; [
          c
          lua
        ])).dependencies;
      };
    in
    "${parsers}/parser";


    # XDG Configuration -----------------
    # Normal LazyVim config here, see https://github.com/LazyVim/starter/tree/main/lua
    xdg.configFile."nvim/lua".source = ./lua;

  };

}

