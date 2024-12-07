{ pkgs, ... }:

{
  # Main settings
  home.username = "shiva";
  home.homeDirectory = "/home/shiva";
  home.stateVersion = "23.11"; # DONT CHANGE THIS
  programs.home-manager.enable = true; # Enable Home-Manager

  # import configuration for apps, packages, etc... .
  imports = [
    (./. + "/fonts")
    (./. + "/apps/git")

    (./. + "/apps/bins")

    (./. + "/apps/cli")

    (./. + "/apps/browsers")

    (./. + "/apps/terminal")
    (./. + "/apps/neovim")
    (./. + "/apps/vscode")
    (./. + "/apps/tmux")
    (./. + "/apps/starship/starship.nix")
    (./. + "/apps/btop")

    (./. + "/apps/hyprland")
    # (./. + "/apps/pyprland")
    (./. + "/apps/hyprlock")

    (./. + "/apps/waybar")
    (./. + "/apps/wlogout")
    (./. + "/themes/gtk/gtk.nix")
    (./. + "/apps/rofi")

    (./. + "/apps/spicetify")
    (./. + "/apps/discord")

    (./. + "/apps/chatterino")

    (./. + "/apps/misc")
  ];

  home.packages = with pkgs; [
    #   :: TERMINAL ::
    zsh
    # tmux-sessionizer
    lf
    eza
    neofetch
    wev # wayland event viewer, for keystrokes
    playerctl
    wl-clipboard
    zsh-autosuggestions
    htop

    #   :: BROWSWERS ::
    # now moved to their own folders
    # firefox-devedition
    # brave
    # google-chrome
    # firefox
    # qutebrowser

    #   :: FONTS ::
    jetbrains-mono
    monaspace
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
    obs-studio-plugins.obs-vkcapture
    obs-studio-plugins.obs-vaapi
    # obs-studio-plugins.obs-multi-rtmp
    audacity

    # uxplay to cast iPad
    uxplay # this depends on system services.avahi
    iptables # required for custom uxplay script
    gst_all_1.gstreamer

    #   :: PROGRAMMING LANGUAGES ::
    just
    lua
    rustup
    python3
    python311Packages.pip
    nodejs
    gcc
    gnumake
    go

    #  :: MISC ::
    zoom-us
    obsidian
    cava
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
