{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./keymaps.nix
    ./coding.nix
    ./editor.nix
    ./ui.nix
    ./style.nix
    ./telescope.nix
    ./treesitter.nix
    ./harpoon.nix
    ./folds.nix
    ./lsp.nix
    ./format.nix
    ./lint.nix
    ./debug.nix
  ];

  config = {
    globals = {
      mapleader = " ";
    };

    clipboard.register = "unnamedplus";

    options = {
      # this changed to opts instead of options ??!!
      # opts = {
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
      gitsigns.enable = true;
      tmux-navigator.enable = true;
      # oil.enable = true;
      # undotree.enable = true;
      fugitive.enable = true;
      # nvim-tree.enable = true;
    };
    extraPlugins = with pkgs; [
      vimPlugins.nvim-web-devicons
      vimPlugins.LazyVim
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
  };
}
