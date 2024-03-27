{
  description = "Shiva's NixOS Flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    # HomeManager Flake
    # this will be passed as an argument for the outputs function by nix
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Other Flakes...
    # /* Testing onedark theme flake
    plugin-onedark = {
      url = "github:navarasu/onedark.nvim";
      flake = false;
    };
    # */

    # /*
    nixvim = {
      # If using a stable channel you can use    url = "github:nix-community/nixvim/nixos-<version>"
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # */

  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
      pkgs = import nixpkgs { system = "x86_64-linux"; config.allowUnfree = true; };
    in
    {
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
          modules = [ (./. + "/home/default.nix") ];

          extraSpecialArgs = { inherit inputs; };
        };
      };

    };
}

