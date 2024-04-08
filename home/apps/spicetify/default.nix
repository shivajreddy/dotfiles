{
  inputs,
  pkgs,
  ...
}: {
  # themable spotify
  imports = [
    # inputs.spicetify-nix.homeManagerModule
    inputs.spicetify-nix
  ];

  programs.spicetify = let
    spicePkgs = inputs.spicetify-nix.packages.${pkgs.system}.default;
    variant = "mocha";
  in {
    enable = true;

    theme = spicePkgs.themes.catppuccin;

    colorScheme = variant;

    enabledExtensions = with spicePkgs.extensions; [
      fullAppDisplay
      history
      genre
      hidePodcasts
      shuffle
    ];
  };
}
