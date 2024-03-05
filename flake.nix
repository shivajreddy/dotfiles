{
  description = "My Flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    # /*  HomeManager -
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # */

  };

  # outputs = { self, nixpkgs, ... }:
  # outputs = inputs@{ self, nixpkgs, ... }:
  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
  # outputs = { self, nixpkgs, home-manager, ... }:
  let 
        lib = nixpkgs.lib; 
        system = "x86_64-linux";
        #pkgs = nixpkgs.legacyPackages.${system};
	#pkgs.config.allowUnfree = true;
        pkgs = import nixpkgs { system = "x86_64-linux"; config.allowUnfree = true;};
	/*
        pkgs = {
	  nixpkgs.legacyPackages.${system};
	  config.allowUnfree = true;
	};
	*/
  in
  {
    nixosConfigurations = {
      predator = lib.nixosSystem {
        # extraSpecialArgs = { inherit inputs; };
        inherit system;
        modules = [ 
          ./predator/configuration.nix 
 	  /* 1.b. Importing home-manager as a module
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.shiva = import ./home_manager/home.nix;
          }
	  # */ 
        ];
      };
      tars = lib.nixosSystem {
        # extraSpecialArgs = { inherit inputs; };
        inherit system;
        modules = [ 
          ./tars/configuration.nix 
 	  /* 1.b. Importing home-manager as a module
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.shiva = import ./home_manager/home.nix;
          }
	  # */ 
        ];
      };
    };

    # /* HomeConfiguration -- using as a package
    homeConfigurations = {
    	predator = home-manager.lib.homeManagerConfiguration {
	  inherit pkgs;
	  modules = [ ./home_manager/home.nix ];
	 
	};
    	shiva = home-manager.lib.homeManagerConfiguration {
	  inherit pkgs;
	  modules = [ ./home_manager/home.nix ];
	 
	};
    };
    # */

  };
}

