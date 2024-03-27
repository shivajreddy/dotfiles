{ config, lib, pkgs, ... }:

{
	imports = [
		./bufferline.nix
	];

	programs.nixvim = {
		enable = true;
	};
}

