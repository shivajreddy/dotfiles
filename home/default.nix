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
    (./. + "/fonts")
    (./. + "/apps/git")

    (./. + "/apps/browsers")

    (./. + "/apps/terminal")
    (./. + "/apps/zsh")
    (./. + "/apps/neovim")
    (./. + "/apps/tmux/tmux.nix")
    (./. + "/apps/starship/starship.nix")

    (./. + "/apps/hyprland")
    (./. + "/apps/hyprlock")
    (./. + "/apps/waybar")
    (./. + "/themes/gtk/gtk.nix")

    (./. + "/apps/spicetify")
    (./. + "/apps/discord")

    (./. + "/apps/chatterino")

    (./. + "/apps/rofi")
  ];

  home.packages = with pkgs; [
    #   :: TERMINAL ::
    zsh
    tmux
    lf
    eza
    neofetch
    wev # wayland event viewer, for keystrokes
    playerctl
    wl-clipboard
    zsh-autosuggestions
    htop
    btop

    #   :: BROWSWERS ::
    # now moved to their own folders
    # brave
    # google-chrome
    # firefox
    # qutebrowser

    #   :: FONTS ::
    jetbrains-mono
    # ibm-plex  # for some reasons this shit wont work, only a sys package

    #  :: LINUX PACKAGES ::
    rofi-wayland
    bluez
    blueman
    pavucontrol
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

    #   :: HYPRLAND ::
    waybar
    bottom
    pyprland
    gtk4
    unzip
    pamixer
    volnoti

    #   :: STREAMING ::
    obs-studio
    obs-studio-plugins.obs-gstreamer
    obs-studio-plugins.obs-vaapi
    uxplay # this depends on system services.avahi
    iptables # required for custom uxplay script
    gst_all_1.gstreamer

    #   :: PROGRAMMING LANGUAGES ::
    just
    lua
    rustup
    python3
    nodejs_21
    gcc
    gnumake
    go

    #  :: MISC ::
    zoom-us
    obsidian
    # vscode
    # jetbrains.clion
    # jetbrains.pycharm-professional
    # jetbrains.rust-rover
    #jetbrains.pycharm-professional
    #jetbrains.webstorm
    # jetbrains.goland
    # spotify
    # spotifyd
    # spotify-tui # this is removed from nixPkgs
    # webcord
  ];
}
