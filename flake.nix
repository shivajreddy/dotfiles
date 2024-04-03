{
  description = "Shiva's NixOS Flake";

  # The nixpkgs entry in the flake registry.
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    # HomeManager Flake
    # this will be passed as an argument for the outputs function by nix
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # NIXVIM
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Other Flakes...
    # /* Testing onedark theme flake
    plugin-onedark = {
      url = "github:navarasu/onedark.nvim";
      flake = false;
    };
    # */
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    lib = nixpkgs.lib;
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      system = "x86_64-linux";
      config.allowUnfree = true;
    };

    nvim_config = {
      extraPackages = with pkgs; [
        # LazyVim
        # lua-language-server
        # stylua
        # Telescope
        # ripgrep
      ];

      extraPlugins = [pkgs.vimPlugins.lazy-nvim];

      extraConfigLua = let
        plugins = with pkgs.vimPlugins; [
          # LazyVim
          LazyVim
          bufferline-nvim
          cmp-buffer
          cmp-nvim-lsp
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
          {
            name = "LuaSnip";
            path = luasnip;
          }
          {
            name = "catppuccin";
            path = catppuccin-nvim;
          }
          {
            name = "mini.ai";
            path = mini-nvim;
          }
          {
            name = "mini.bufremove";
            path = mini-nvim;
          }
          {
            name = "mini.comment";
            path = mini-nvim;
          }
          {
            name = "mini.indentscope";
            path = mini-nvim;
          }
          {
            name = "mini.pairs";
            path = mini-nvim;
          }
          {
            name = "mini.surround";
            path = mini-nvim;
          }
        ];
        mkEntryFromDrv = drv:
          if lib.isDerivation drv
          then {
            name = "${lib.getName drv}";
            path = drv;
          }
          else drv;
        lazyPath = pkgs.linkFarm "lazy-plugins" (builtins.map mkEntryFromDrv plugins);
      in ''
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
            -- disable mason.nvim, use config.extraPackages
            { "williamboman/mason-lspconfig.nvim", enabled = false },
            { "williamboman/mason.nvim", enabled = false },
            -- uncomment to import/override with your plugins
            -- { import = "plugins" },
            -- put this line at the end of spec to clear ensure_installed
            { "nvim-treesitter/nvim-treesitter", opts = { ensure_installed = {} } },
          },
        })
      '';
    };

    nixvim_custom = inputs.nixvim.legacyPackages.${system};
    nvim = nixvim_custom.makeNixvim nvim_config;
  in {
    nixosConfigurations = {
      predator = lib.nixosSystem {
        # extraSpecialArgs = { inherit inputs; };
        inherit system;
        modules = [
          (./. + "/hosts/predator/configuration.nix")
        ];
      };
      tars = lib.nixosSystem {
        # extraSpecialArgs = { inherit inputs; };
        inherit system;
        modules = [
          (./. + "/hosts/tars/configuration.nix")
        ];
      };
    };

    # HomeConfiguration -- using as a package
    homeConfigurations = {
      shiva = inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          (./. + "/home/default.nix")
          inputs.nixvim.homeManagerModules.nixvim
        ];

        extraSpecialArgs = {inherit inputs;};
      };
    };
  };
}
