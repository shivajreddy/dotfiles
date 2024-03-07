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
	(./. + "/fonts/fonts.nix")
	(./. + "/apps/git.nix")
	(./. + "/apps/hyprland/hyprland.nix")
	(./. + "/apps/gtk.nix")
	(./. + "/apps/kitty.nix")
	(./. + "/apps/starship.nix")
  ];
  home.packages = with pkgs; [
	# core
	zsh
	firefox
	brave
	google-chrome
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
  ];

  # Enable spotify-deamon
  services = {
    spotifyd.enable = true;
  };

  programs.direnv.enable = true;
  programs.direnv.enableZshIntegration = true;
  programs.direnv.nix-direnv.enable = true;
}
