{ config, lib, pkgs, inputs, ... }:

# the inputs are passed in as 'extraSpecialArgs' in my flake file


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

  # Build plugins from github
  huez-nvim = pkgs.vimUtils.buildVimPlugin { name = "huez.nvim"; src = inputs.huez-nvim; };
  blame-me-nvim = pkgs.vimUtils.buildVimPlugin { name = "blame-me.nvim"; src = inputs.blame-me-nvim; };
  cmake-tools-nvim = pkgs.vimUtils.buildVimPlugin { name = "cmake-tools.nvim"; src = inputs.cmake-tools-nvim; };
  cmake-gtest-nvim = pkgs.vimUtils.buildVimPlugin { name = "cmake-gtest.nvim"; src = inputs.cmake-gtest-nvim; };

  mkEntryFromDrv = drv:
    if lib.isDerivation drv then { name = "${lib.getName drv}"; path = drv; }
    else
      drv;
in
{

  # /* Learning nvim setup using flake from: https://www.youtube.com/watch?v=YZAnJ0rwREA&t=210s
  nixpkgs = {
    overlays = [
      # 1st arg is what you get, so its like the final nixpkgs you would get after sending in
      # the prev nixpkgs
      (final: prev: {
        # we are extending the vimPlugins set with our custom plugins
        # // is the update operator, so its like appending in this case cuz onedark isn't there in vimPlugins
        vimPlugins = prev.vimPlugins // {
          shivas-onedark-nvim = prev.vimUtils.buildVimPlugin {
            name = "onedark";
            src = inputs.plugin-onedark; # this is the name we gave in the inputs set of flake.nix file
          };
        };
      })
    ];
  };
  # */

  programs.neovim = {
    enable = true;

    extraPackages = with pkgs; [
      nil # nix language server
      lua-language-server
      yaml-language-server
      rust-analyzer # rust LSP
      pyright
    ];

    extraLuaConfig = ''
      ${builtins.readFile ./options.lua}
    '';

    plugins = with pkgs.vimPlugins; [
      LazyVim
      neodev-nvim
      telescope-fzf-native-nvim
      cmp_luasnip
      cmp-nvim-lsp
      luasnip
      friendly-snippets
      lualine-nvim
      nvim-web-devicons
      vim-nix
      tmux-navigator
      nvim-cmp
      {
        plugin = catppuccin-nvim;
        config = toLuaFile ./plugin/theme.lua;
      }
      {
        plugin = neo-tree-nvim;
        config = toLuaFile ./plugin/neotree.lua;
      }
      {
        plugin = nvim-lspconfig;
        config = toLuaFile ./plugin/lsp.lua;
      }
      {
        plugin = comment-nvim;
        config = toLua "require(\"Comment\").setup()";
      }
      {
        plugin = nvim-cmp;
        config = toLuaFile ./plugin/cmp.lua;
      }
      {
        plugin = telescope-nvim;
        config = toLuaFile ./plugin/telescope.lua;
      }
      {
        plugin = (nvim-treesitter.withPlugins (p: [
          p.tree-sitter-nix
          p.tree-sitter-vim
          p.tree-sitter-bash
          p.tree-sitter-lua
          p.tree-sitter-python
          p.tree-sitter-json
        ]));
        config = toLuaFile ./plugin/treesitter.lua;
      }

    ];

  };
}

