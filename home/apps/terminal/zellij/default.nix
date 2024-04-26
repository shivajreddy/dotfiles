{ ... }:

# let
#  confFileContent = builtins.readFile (./. + "/kitty.conf");
# in
{
	imports = [];

  programs.zellij.enable = true;

  # Copy the theme folder 
  # xdg.configFile."kitty/themes".source = ./themes;
}

