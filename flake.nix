{
  description = "My Flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    # ------------ HomeManager module IMPORT ------------ #
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  # outputs = { self, nixpkgs, ... }:
  outputs = inputs@{ self, nixpkgs, ... }:
  # outputs = inputs@{ self, nixpkgs, home-manager, ... }:
  # outputs = { self, nixpkgs, home-manager, ... }:
  let 
        lib = nixpkgs.lib; 
        system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    # -------------------- HOME config -------------------- #.

    nixosConfigurations = {
      predator = lib.nixosSystem {
        # extraSpecialArgs = { inherit inputs; };
        inherit system;
        modules = [ 
          ./predator/configuration.nix 
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.shiva = import ./home_manager/home.nix;
          }
        ];
      };
      tars = lib.nixosSystem {
        # extraSpecialArgs = { inherit inputs; };
        inherit system;
        modules = [ 
          ./tars/configuration.nix 
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.shiva = import ./home_manager/home.nix;
          }
        ];
      };
    };

    /* HomeConfiguration
    homeConfigurations = {
    	shiva = home-manager.lib.homeManagerConfiguration {
	  inherit pkgs;
	  modules = [ ./home.nix ];
	};
    };
    # */

  };
}

