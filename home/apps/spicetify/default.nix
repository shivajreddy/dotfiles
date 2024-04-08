{ inputs, pkgs, ... }: 
let
  spicePkgs = inputs.spicetify-nix.packages.${pkgs.system}.default;
in 
{
  # themable spotify
  imports = [
    inputs.spicetify-nix.homeManagerModule
    # inputs.spicetify-nix
  ];

  programs.spicetify = {
    enable = true;

    theme = spicePkgs.themes.catppuccin;

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
