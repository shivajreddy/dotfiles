{ config, lib, pkgs, ... }:
{
    programs.neovim-nightly = {
    # programs.neovim = {

    enable = true;

    extraPackages = with pkgs; [
      # LazyVim
      lua-language-server
      stylua
      # Telescope
      ripgrep

      /* These worked with nixvim
       * # so look at https://github.com/nix-community/nixvim/blob/main/plugins/lsp/language-servers/default.nix
      bashls.enable = true;
      clangd.enable = true;
      cmake.enable = true;
      # cssls.enable = true;
      # html.enable = true;
      lua-ls.enable = true;
      nixd.enable = true;
      # ruff-lsp.enable = true;
      pyright.enable = true;
      rust-analyzer = {
        enable = true;
        installCargo = true;
        installRustc = true;
      };
      # tsserver.enable = true;
      yamlls.enable = true;
      # zls.enable = true;
      # */
      nodePackages.bash-language-server
      clang-tools
      cmake-language-server
      pyright
      rust-analyzer
      rustc
      cargo

      gopls
      golangci-lint-langserver
    ];

    plugins = with pkgs.vimPlugins; [
      lazy-nvim
    ];

    extraLuaConfig =
      let
        # NOTE: 1. These are the plugins that we are using nix to install directly instead of
        # lazyvim to install them, since they are used in lazyvim config, and which would get 
        # executed when require("lazy").setup({spec= { "LazyVim/LazyVim", import = "lazyvim.plugins" }, }) 
        # is called and this is called in this extraLuaConfig attribute for 'neovim' program
        plugins = with pkgs.vimPlugins; [
          # LazyVim
          LazyVim
          bufferline-nvim
          cmp-buffer
          cmp-nvim-lsp
          cmp-nvim-lua
          cmp-cmdline
          cmp-path
          cmp_luasnip
          conform-nvim
          dashboard-nvim
          dressing-nvim
          flash-nvim
          friendly-snippets
          gitsigns-nvim
          indent-blankline-nvim
          lualine-nvim
          neo-tree-nvim
          neoconf-nvim
          neodev-nvim
          noice-nvim
          nui-nvim
          nvim-cmp
          nvim-lint
          nvim-lspconfig
          nvim-notify
          nvim-spectre
          nvim-treesitter
          nvim-treesitter-context
          nvim-treesitter-textobjects
          nvim-ts-autotag
          nvim-ts-context-commentstring
          nvim-web-devicons
          persistence-nvim
          plenary-nvim
          telescope-fzf-native-nvim
          telescope-nvim
          todo-comments-nvim
          tokyonight-nvim
          trouble-nvim
          vim-illuminate
          vim-startuptime
          which-key-nvim
          { name = "LuaSnip"; path = luasnip; }
          { name = "catppuccin"; path = catppuccin-nvim; }
          { name = "mini.ai"; path = mini-nvim; }
          { name = "mini.bufremove"; path = mini-nvim; }
          { name = "mini.comment"; path = mini-nvim; }
          { name = "mini.indentscope"; path = mini-nvim; }
          { name = "mini.pairs"; path = mini-nvim; }
          { name = "mini.surround"; path = mini-nvim; }

          obsidian-nvim
        ];
        mkEntryFromDrv = drv:
          if lib.isDerivation drv then
            { name = "${lib.getName drv}"; path = drv; }
          else
            drv;
        lazyPath = pkgs.linkFarm "lazy-plugins" (builtins.map mkEntryFromDrv plugins);
      in
      # NOTE 2: These settings are important to ensure lazyvim is setup properly for nix
      # Especially, the treesitter and LSP should be installed through nix, not lazyvim
      # we are installing the plugins also through nix (however if we werent installing any or some), those 
      # would be installed by lazy, but the reason to install them trhrough nix is because of caching done by nix,
      # and it makes it faster to install them the nix way.
      ''
        require("lazy").setup({
          defaults = {
            lazy = true,
          },
          dev = {
            -- reuse files from pkgs.vimPlugins.*
            path = "${lazyPath}",
            patterns = { "." },
            -- fallback to download
            fallback = true,
          },
          spec = {
            { "LazyVim/LazyVim", import = "lazyvim.plugins" },
            -- The following configs are needed for fixing lazyvim on nix
            -- force enable telescope-fzf-native.nvim
            { "nvim-telescope/telescope-fzf-native.nvim", enabled = true },
            -- disable mason.nvim, use programs.neovim.extraPackages
            { "williamboman/mason-lspconfig.nvim", enabled = false },
            { "williamboman/mason.nvim", enabled = false },
            -- import/override with your plugins
            { import = "plugins" },
            -- treesitter handled by xdg.configFile."nvim/parser", put this line at the end of spec to clear ensure_installed
            { "nvim-treesitter/nvim-treesitter", opts = { ensure_installed = {} } },
          },
        })
      '';
  };

  # NOTE 3: make sure to given an emtpty table as the input for 'ensure_installed' in the nvim-treesitter setup, that
  # you can see in the above extraLuaConfig attribute, this ensures that the nvim-treesitter wont install any treesitter
  # plugins, and now we can 1. install them trhough nix and 2. symlink them using xdg
  # https://github.com/nvim-treesitter/nvim-treesitter#i-get-query-error-invalid-node-type-at-position
  xdg.configFile."nvim/parser".source =
    let
      parsers = pkgs.symlinkJoin {
        name = "treesitter-parsers";
        paths = (pkgs.vimPlugins.nvim-treesitter.withPlugins (plugins: with plugins; [
          c
          lua
          cpp
          rust
          python
          go
        ])).dependencies;
      };
    in
    "${parsers}/parser";

  # NOTE 4: This makes our custom lua config files, which is the beaty of just using lua files the regular way.
  # the './lua' (right hand side value in below code) folder next to this file is nix file will be linked to
  # '.config/nvim/lua' this is the left hand side value of code below
  # Normal LazyVim config here, see https://github.com/LazyVim/starter/tree/main/lua
  # NOTE 5: Finish the setup for the lsp's in lua/plugins/lsp.lua
  xdg.configFile."nvim/lua".source = ./lua;
}

