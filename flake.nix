{
  description = "Shiva's NixOS Flake";

  # The nixpkgs entry in the flake registry.
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    # 1. Mention which overlay flakes we are using
    # THIS IS NVIM nigtly, which is the dev branch
    # neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";

    # HomeManager Flake
    # this will be passed as an argument for the outputs function by nix
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };


    # Other Flakes...
    spicetify-nix = {
      url = "github:the-argus/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Mange secrets
    agenix.url = "github:ryantm/agenix";

    # keymapper
    xremap-flake.url = "github:xremap/nix-flake";

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
      # 2. Mention all your overlays here
      overlays = [
        # inputs.neovim-nightly-overlay.overlay
      ];
    };
  in {

    # NIXOS
    nixosConfigurations = {

      # T A R S
      tars = lib.nixosSystem {
        specialArgs = { inherit inputs; };
        inherit system;
        modules = [
          (./. + "/hosts/tars/configuration.nix")
        ];
      };

      # P R E D A T O R
      predator = lib.nixosSystem {
        # extraSpecialArgs = { inherit inputs; };
        inherit system;
        modules = [
          (./. + "/hosts/predator/configuration.nix")
        ];
      };

    };

    # HOMEMANAGER
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
