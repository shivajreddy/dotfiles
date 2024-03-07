
{ lib, ... }:
{
	imports = [];

	programs.starship.enable = true;
	# programs.starship.enableZshIntegration = true;

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
