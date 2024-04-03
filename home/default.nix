{
  config,
  pkgs,
  inputs,
  ...
}: let
  neovimconfig = import ./apps/nixvim;
  nvim = inputs.nixvim.legacyPackages.x86_64-linux.makeNixvim neovimconfig;
  /*
  nvim = inputs.nixvim.legacyPackages.x86_64-linux.makeNixvimWithModule {
    inherit pkgs;
    module = neovimconfig;
  };
  */
in {
  # Main settings
  home.username = "shiva";
  home.homeDirectory = "/home/shiva";
  home.stateVersion = "23.11"; # DONT CHANGE THIS
  programs.home-manager.enable = true; # Enable Home-Manager

  # import configuration for apps, packages, etc...
  imports = [
    (./. + "/fonts/fonts.nix")
    (./. + "/apps/git.nix")
    (./. + "/apps/zsh.nix")
    (./. + "/apps/kitty/kitty.nix")
    (./. + "/apps/tmux/tmux.nix")
    (./. + "/apps/alacritty/alacritty.nix")
    (./. + "/apps/starship/starship.nix")
    (./. + "/apps/hyprland/hyprland.nix")
    (./. + "/apps/waybar/waybar.nix")
    (./. + "/themes/gtk/gtk.nix")
    # (./. + "/themes/qt/qt.nix")

    # (./. + "/apps/nvim/default.nix")
    # (./. + "/apps/nixvim/default.nix")
  ];

  # programs.nixvim.enable = true;

  home.packages = with pkgs; [
    zsh
    nvim

    firefox
    brave
    google-chrome
    qutebrowser

    kitty
    starship
    alacritty

    jetbrains-mono
    rofi-wayland
    bluez
    blueman
    pavucontrol
    htop
    btop
    swww
    localsend

    xfce.thunar
    xfce.thunar-volman
    xfce.tumbler
    nomacs

    # Streaming
    obs-studio
    obs-studio-plugins.obs-gstreamer
    obs-studio-plugins.obs-vaapi
    uxplay # this depends on system services.avahi
    iptables # required for custom uxplay script
    gst_all_1.gstreamer

    neofetch
    wev # wayland event viewer, for keystrokes
    playerctl
    wl-clipboard
    zsh-autosuggestions

    waybar

    eza
    bottom
    pyprland
    lf
    gcc
    gnumake
    ripgrep #NOTE: may be this is being installed twice
    spotify
    # spotifyd
    # spotify-tui
    discord
    gtk4
    pamixer
    unzip

    zoom-us

    obsidian
    tmux
    # vscode
    # jetbrains.clion
    # jetbrains.pycharm-professional
    # jetbrains.rust-rover
    #jetbrains.pycharm-professional
    #jetbrains.webstorm

    # lua
    # rust-analyzer
    # rnix-lsp
    # rustup
    python3
    nodejs_21
  ];
}
