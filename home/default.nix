{ config, pkgs, ... }:

{
  home.username = "shiva";
  home.homeDirectory = "/home/shiva";

  home.stateVersion = "23.11"; # DONT CHANGE THIS

  # wtf is this
  # home.pointerCursor.gtk.enable = true;

  # /* Enable Home-Manager
  programs.home-manager.enable = true;
  # */ 

  # import configuration for apps, packages, etc...
  imports = [
	(./. + "/fonts/fonts.nix")
	(./. + "/apps/git.nix")
	(./. + "/apps/zsh.nix")
	(./. + "/apps/kitty/kitty.nix")
	(./. + "/apps/alacritty/alacritty.nix")
	(./. + "/apps/starship/starship.nix")
	(./. + "/apps/hyprland/hyprland.nix")
	(./. + "/apps/waybar/waybar.nix")
	(./. + "/themes/gtk/gtk.nix")
#
	# (./. + "/themes/qt/qt.nix")
  ];
  home.packages = with pkgs; [
	# core
	zsh
	firefox
	brave
	google-chrome
	kitty
	alacritty
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
	# spotifyd
	# spotify-tui
	gtk4
	pamixer
	unzip
  spicetify-cli

	tmux
	vscode
	#jetbrains.pycharm-professional
	#jetbrains.webstorm

  gnome-themes-extra
  # catppuccin-gtk

	lua
	rustup
	python3
	nodejs_21

  ];

  services = {
    # spotifyd.enable = true; 
  };

}
