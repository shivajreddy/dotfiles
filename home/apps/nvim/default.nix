{ config, lib, ... }:

{

	programs.neovim = {
		enable = true;

		# ...

		extraLuaConfig = ''

		${builtins.readFile ./. + "/options.lua"}

		'';

	};
}

