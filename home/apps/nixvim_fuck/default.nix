{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./keymaps.nix
		./editor.nix
		./style.nix
		./telescope.nix
		./coding.nix
		./lsp.nix
		./ui.nix
  ];

  config = {
    globals = {
      mapleader = " ";
    };

    clipboard.register = "unnamedplus";

    opts = {
      number = true;
      # colorcolumn = "80";
      relativenumber = true;
      shiftwidth = 2;
      tabstop = 2;
      wrap = false;
      swapfile = false; #Undotree
      backup = false; #Undotree
      undofile = true;
      hlsearch = true;
      incsearch = true;
      termguicolors = true;
      scrolloff = 8;
      signcolumn = "yes";
      updatetime = 50;
      foldlevelstart = 99;
    };

    plugins = {
			lazy.enable = true;
    };

		# VimPlugins that are not there on NixVim
    extraPlugins = with pkgs; [
      vimPlugins.nvim-web-devicons
      vimPlugins.telescope-fzf-native-nvim
    ];
	
    extraPackages = with pkgs; [
      # Formatters
      alejandra
      asmfmt
      astyle
      black
      cmake-format
      gofumpt
      golines
      gotools
      isort
      nodePackages.prettier
      prettierd
      rustfmt
      shfmt
      stylua
      # Linters
      commitlint
      eslint_d
      golangci-lint
      hadolint
      html-tidy
      luajitPackages.luacheck
      markdownlint-cli
      nodePackages.jsonlint
      pylint
      shellcheck
      vale
      yamllint
      # Debuggers / misc deps
      asm-lsp
      bashdb
      clang-tools
      delve
      fd
      gdb
      lldb_17
      llvmPackages_17.bintools-unwrapped
      marksman
      python3
      ripgrep
      rr
    ];

    # /*
    extraConfigLua = ''
        -- require("neoconf").setup({})
        require("lazy").setup({
          defaults = {
            lazy = true,
          },
          spec = {
            -- This ensures that lazyvim will install all the plugins
            -- Should you use lazyvim to set up the plugins for you?
            { "LazyVim/LazyVim", import = "lazyvim.plugins" },

            -- The following configs are needed for fixing lazyvim on nix
            -- force enable telescope-fzf-native.nvim
            { "nvim-telescope/telescope-fzf-native.nvim", enabled = true },

            -- disable mason.nvim, use config.extraPackages
            { "williamboman/mason-lspconfig.nvim", enabled = false },
            { "williamboman/mason.nvim", enabled = false },

            -- uncomment to import/override with your plugins
            -- Use xdg to copy the ./cnfig/nvim folder in which you have directories like this https://github.com/LazyVim/starter
            { import = "plugins" },

            -- put this line at the end of spec to clear ensure_installed
            -- { "nvim-treesitter/nvim-treesitter", opts = { ensure_installed = {} } },
          },
        })
    '';
    # */

  };

}

