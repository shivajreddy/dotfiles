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

    theme = spicePkgs.themes.Dribbblish;
    colorScheme = "gruvbox";

    enabledExtensions = with spicePkgs.extensions; [
      fullAppDisplay
      lastfm
      # genre
      historyShortcut
      shuffle
    ];
  };
}
