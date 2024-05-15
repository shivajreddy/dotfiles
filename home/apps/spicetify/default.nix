{ inputs, pkgs, ... }: 
let
  spicePkgs = inputs.spicetify-nix.packages.${pkgs.system}.default;
in 
{
  # themable spotify
  imports = [
    inputs.spicetify-nix.homeManagerModule
  ];

  programs.spicetify = {
    enable = true;

    # theme = spicePkgs.themes.catppuccin;
    # colorScheme = "mocha";

    # theme = spicePkgs.themes.Comfy;
    theme = spicePkgs.themes.text;
    colorScheme = "rosepine";

    enabledExtensions = with spicePkgs.extensions; [
      fullScreen
      groupSession
      keyboardShortcut
    ];
  };
}
