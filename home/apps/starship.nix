
{ lib, ... }:
{
	imports = [];

	programs.starship.enable = true;

	home.sessionVariables = {
	    STARSHIP_CONFIG = "~/.config/starship/config.toml";
	};

	programs.starship.settings = {
		add_newline = true;
		format = lib.concatStrings [
		"$line_break"
		"$package"
		"$line_break"
		"$character"
		];
		scan_timeout = 10;
		character = {
			success_symbol = "$";
			error_symbol = "$";
		};
	};
}
