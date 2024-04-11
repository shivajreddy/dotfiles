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

    theme = spicePkgs.themes.Dribblish;
    colorScheme = "mocha";

    enabledExtensions = with spicePkgs.extensions; [
      fullAppDisplay
      # history
      # genre
      # hidePodcasts
      # shuffle
    ];
  };
}
