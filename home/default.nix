{ config, pkgs, ... }:

{
  home.username = "shiva";
  home.homeDirectory = "/home/shiva";

  home.stateVersion = "23.11"; # DONT CHANGE THIS

  # /* Enable Home-Manager
  programs.home-manager.enable = true;
  # */ 

  # import configuration for apps, packages, etc...
  imports = [
	(./. + "/apps/hyprland/hyprland.nix")
	(./. + "/apps/kitty.nix")
	(./. + "/apps/starship.nix")
  ];

  # Configuration of packages using Home-Manager ???
  # NOTE : write/make a file on left ,with the content of the file on right
  # home.file.".config/hypr/hyprland.conf".source = ./apps/hyprland.conf


  # Fonts
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-Bold.ttf".source = ./fonts/berkeley-mono/BerkeleyMono-Bold.ttf;
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-BoldItalic.ttf".source = ./fonts/berkeley-mono/BerkeleyMono-BoldItalic.ttf;
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-Italic.ttf".source = ./fonts/berkeley-mono/BerkeleyMono-Italic.ttf;
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-Regular.ttf".source = ./fonts/berkeley-mono/BerkeleyMono-Regular.ttf;

  home.file.".local/share/fonts/sf-san-francisco-pro".source = ./fonts/san-francisco-pro;


  home.packages = with pkgs; [
	# core
	zsh
	firefox
	brave
	kitty
	neovim
	jetbrains-mono
	rofi-wayland
	bluez
	blueman
	pavucontrol
	htop
	btop
	swww
	xfce.thunar
	# gvfs  # for thumbdrive to work with thunar, not working
	xfce.thunar-volman
	xfce.tumbler
	nomacs
	obs-studio
	starship
	neofetch
	wev	# wayland event viewer, for keystrokes
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
	ripgrep
	spotify
	spotifyd
	spotify-tui
	lua

	gtk4

	pamixer

	unzip

	#p7zip
	#gnome.file-roller
	#gnome.nautilus
	#nautilus-open-any-terminal
	#webp-pixbuf-loader
	#ffmpegthumbnailer
  ];

  # Enable spotify-deamon
  services = {
    #udisk2.enable = true;
    spotifyd.enable = true;

    #tumbler.enable = true;
  };


  #also, path to python is ${pkgs.python}/bin/python
  # configuration of my programs
  #/*
  gtk = {
    enable = true;
    theme = {
      name = "Catppuccin-Mocha-Compact-Pink-Dark";
      package = pkgs.catppuccin-gtk.override {
	accents = [ "pink" ];
	size = "standard";
	tweaks = [ "rimless" ];
	variant = "mocha";
      };
    };
  };
  # */

  /* Remove existing GTK settings to avoid conflicts
  home.file.".config/gtk-3.0/settings.ini".state = "absent";
  home.file.".config/gtk-4.0/settings.ini".state = "absent";
  # */

  /* After ensuring the existing settings are absent, recreate them with the desired theme settings
  home.file.".config/gtk-3.0/settings.ini".text = ''
    [Settings]
    gtk-theme-name="${config.gtk.theme.name}"
    gtk-icon-theme-name="YourIconTheme" # Replace with your icon theme
    gtk-font-name="YourFont 11" # Replace with your font settings
  '';
  home.file.".config/gtk-4.0/settings.ini".text = ''
    [Settings]
    gtk-theme-name="${config.gtk.theme.name}"
    gtk-icon-theme-name="YourIconTheme" # Replace with your icon theme
    gtk-font-name="YourFont 11" # Replace with your font settings
  '';
  */

  # Linking the Theme assets
  /*
  xdg.configFile = {
    "gtk-4.0/assets".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
    "gtk-4.0/gtk.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
    "gtk-4.0/gtk-dark.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
  };
  */

  programs.git = {
    enable = true;
    userName = "shivajreddy";
    userEmail = "shivajreddy@outlook.com";
    extraConfig.credential.helper = "store";
    # Note: first time using git, when pushing to your account,
    # use username, for password paste the token from github.com 
  };

  home.sessionVariables = {
    STARSHIP_CONFIG = "~/.config/starship/config.toml";
  };

  # --------Directly pasting it
  programs.starship.enable = true;

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    shellAliases = {
    vi="nvim";
    ls = "eza --icons -l -T -L=1";
    vihome="vi /home/shiva/dotfiles/home/default.nix";
    htop = "btm";
    gaa="git add .";
    gst="git status .";
    gcmsg="git commit -m";
    ggpush="git push -u origin main";
    ggpull="git pull -u origin main";
    };
    # RPROMPT="%F{red}▂%f%F{yellow}▄%f%F{green}▆%f%F{cyan}█%f%F{blue}▆%f%F{magenta}▄%f%F{white}▂%f"
  };
    /*
    initExtra = ''
    PROMPT="%F{magenta}%n%f%u@%U%F{blue}%m%f%u:%F{yellow}%~%f
     %F{green}%f "
    [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
    '';
    */

  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellAliases = {
    };
  };


  programs.direnv.enable = true;
  programs.direnv.enableZshIntegration = true;
  programs.direnv.nix-direnv.enable = true;
}
