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
	#./zsh.nix # My zsh and bash config
	# /home/shiva/dotfiles/home_manager/shell/zsh.nix
  ];

  # Configuration of packages using Home-Manager ???
  # NOTE : write/make a file on left ,with the content of the file on right
  # home.file.".config/hypr/hyprland.conf".source = ./apps/hyprland.conf


# /* Fonts
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-Bold.ttf".source = ./fonts/berkeley-mono/BerkeleyMono-Bold.ttf;
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-BoldItalic.ttf".source = ./fonts/berkeley-mono/BerkeleyMono-BoldItalic.ttf;
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-Italic.ttf".source = ./fonts/berkeley-mono/BerkeleyMono-Italic.ttf;
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-Regular.ttf".source = ./fonts/berkeley-mono/BerkeleyMono-Regular.ttf;

  home.file = {
    ".local/share/fonts/sf-san-francisco-pro".source = ./fonts/san-francisco-pro;
    # Ensure you have this line to indicate copying the directory instead of linking
    ".local/share/fonts/sf-san-francisco-pro".policy = "copy";
  };

  

# */

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
	swayosd
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

  home.file = {
    /*
    "mydir2/test.vim".text = ''
      this is a test file that is created using home-manager
    '';
    */
  };

#also, path to python is ${pkgs.python}/bin/python

  # configuration of my programs
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
    cat = "bat";
    htop = "btm";
    fd = "fd -Lu";
    w3m = "w3m -no-cookie -v";
    neofetch = "disfetch";
    fetch = "disfetch";
    gitfetch = "onefetch";
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
    vi="nvim";
    ls = "eza --icons -l -T -L=1";
    cat = "bat";
    htop = "btm";
    fd = "fd -Lu";
    w3m = "w3m -no-cookie -v";
    neofetch = "disfetch";
    fetch = "disfetch";
    gitfetch = "onefetch";
    };
  };


  programs.direnv.enable = true;
  programs.direnv.enableZshIntegration = true;
  programs.direnv.nix-direnv.enable = true;
}
