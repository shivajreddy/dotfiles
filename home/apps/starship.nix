
{ ... }:
{
	imports = [];

	programs.starship.enable = true;
	programs.starship.enableZshIntegration = true;
	programs.starship.settings = {
		format = lib.concatStrings [
			[╭](surface2)\
$directory\
$username\
$hostname\
$container\
\
$git_branch\
$git_commit\
$git_state\
$git_status\
$git_metrics\
$hg_branch\
$vcsh\
\
$cmd_duration\
$jobs\
$sudo\
$shlvl\
$status\
\
$buf\
$bun\
$c\
$cmake\
$cobol\
$crystal\
$dart\
$deno\
$dotnet\
$elixir\
$elm\
$erlang\
$fennel\
$golang\
$haskell\
$haxe\
$helm\
$java\
$julia\
$kotlin\
$lua\
$nim\
$nodejs\
$ocaml\
$perl\
$php\
$purescript\
$python\
$red\
$ruby\
$rust\
$scala\
$spack\
$swift\
$vagrant\
$vlang\
$zig\
\
(${env_var.incognito}\
$package\
$nix_shell\
$aws\
$azure\
$docker_context\
$kubernetes\
$terraform)\
\
$line_break\
\
$shell\
[╰](surface2)\
$character
		];
	};

}
