{
  description = "Shiva's NixOS Flake, with nixvim ";

  # The nixpkgs entry in the flake registry.
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    # HomeManager Flake
    # this will be passed as an argument for the outputs function by nix
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    /* NIXVIM
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # */

    neovim-nightly.url = "github:neovim/neovim?dir=contrib";
    neovim-nightly.inputs.nixpkgs.follows = "nixpkgs";

    # Plugins not available in nixpkgs
    huez-nvim = { url = "github:vague2k/huez.nvim"; flake = false; };
    blame-me-nvim = { url = "github:hougesen/blame-me.nvim"; flake = false; };
    cmake-tools-nvim = { url = "github:Civitasv/cmake-tools.nvim"; flake = false; };
    cmake-gtest-nvim = { url = "github:hfn92/cmake-gtest.nvim"; flake = false; };

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

    # Derivation containing all plugins
    pluginPath = import ./plugins.nix { inherit pkgs lib inputs; };

    # Derivation containing all runtime dependencies
    runtimePath = import ./runtime.nix { inherit pkgs; };

    # Link together all treesitter grammars into single derivation
    treesitterPath = pkgs.symlinkJoin {
      name = "lazyvim-nix-treesitter-parsers";
      paths = pkgs.vimPlugins.nvim-treesitter.withAllGrammars.dependencies;
    };

    # Use nightly neovim only ;)
    neovimNightly = inputs.neovim-nightly.packages.${system}.default;
    # Wrap neovim with custom init and plugins
    neovimWrapped = pkgs.wrapNeovim neovimNightly {
      configure = {
        customRC = /* vim */ ''
          " Populate paths to neovim
          let g:config_path = "${./config}"
          let g:plugin_path = "${pluginPath}"
          let g:runtime_path = "${runtimePath}"
          let g:treesitter_path = "${treesitterPath}"
          " Begin initialization
          source ${./config/init.lua}
        '';
        packages.all.start = [ pkgs.vimPlugins.lazy-nvim ];
      };
    };

  in {


    nixosConfigurations = {
      predator = lib.nixosSystem {
        # extraSpecialArgs = { inherit inputs; };
        inherit system;
        modules = [
          (./. + "/hosts/predator/configuration.nix")
        ];

        packages = {
          # Wrap neovim again to make runtime dependencies available
          nvim = pkgs.writeShellApplication {
            name = "nvim";
            runtimeInputs = [ runtimePath ];
            text = ''${neovimWrapped}/bin/nvim "$@"'';
          };
        };

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
          # inputs.nixvim.homeManagerModules.nixvim
        ];

        extraSpecialArgs = {inherit inputs;};
      };
    };

  };
}
