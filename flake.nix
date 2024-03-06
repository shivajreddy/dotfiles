{
  description = "Shiva's NixOS Flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    # /*  HomeManager -
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # */

  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
  let 
        lib = nixpkgs.lib; 
        system = "x86_64-linux";
        pkgs = import nixpkgs { system = "x86_64-linux"; config.allowUnfree = true;};
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
    	shiva = home-manager.lib.homeManagerConfiguration {
	  inherit pkgs;
	  modules = [ ( ./. + "/home/default.nix" ) ];
	};
    };

  };
}

