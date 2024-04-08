{
  config,
  pkgs,
  inputs,
  ...
}: let
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

    (./. + "/apps/kitty")
    (./. + "/apps/zsh")
    (./. + "/apps/neovim")
    (./. + "/apps/tmux/tmux.nix")
    (./. + "/apps/starship/starship.nix")

    (./. + "/apps/hyprland/hyprland.nix")
    (./. + "/apps/waybar")
    (./. + "/themes/gtk/gtk.nix")

    (./. + "/apps/chatterino")
  ];

  home.packages = with pkgs; [
    zsh

    firefox
    brave
    google-chrome
    qutebrowser

    # kitty
    # starship

    jetbrains-mono
    rofi-wayland
    bluez
    blueman
    pavucontrol
    htop
    btop
    swww
    swaynotificationcenter
    swaylock
    localsend

    xfce.thunar
    xfce.thunar-volman
    xfce.tumbler
    nomacs
    grim
    slurp
    swappy

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
    spotify
    # spotifyd
    # spotify-tui
    # discord
    webcord
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

    lua
    # rust-analyzer
    # rnix-lsp
    rustup
    python3
    nodejs_21
  ];

  /*
  xdg.configFile."nvim" = {
    source = config.lib.file.mkOutOfStoreSymlink ./apps/nixvim_fuck/config;
  };
  # */
}
