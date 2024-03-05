{ config, pkgs, ... }:

{
  home.username = "shiva";
  home.homeDirectory = "/home/shiva";

  home.stateVersion = "23.11"; # DONT CHANGE THIS

  # /* Enable Home-Manager
  programs.home-manager = true;
  # */ 

  # import configuration for apps, packages, etc...
  imports = [
	#./zsh.nix # My zsh and bash config
	# /home/shiva/dotfiles/home_manager/shell/zsh.nix
  ];

  home.packages = [
    zsh
    kitty
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
		gvfs  # for thumbdrive to work with thunar, not working
		xfce.thunar-volman
		xfce.tumbler
		nomacs
		spotify
		obs-studio
		starship
		neofetch
		wev	# wayland event viewer, for keystrokes
		playerctl
		swayosd
		wl-clipboard
		zsh-autosuggestions
		eza
  ];

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
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # --------Directly pasting it
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    shellAliases = {
    ls = "eza --icons -l -T -L=1";
    cat = "bat";
    htop = "btm";
    fd = "fd -Lu";
    w3m = "w3m -no-cookie -v";
    neofetch = "disfetch";
    fetch = "disfetch";
    gitfetch = "onefetch";
    };
    initExtra = ''
    PROMPT=" ◉ %U%F{magenta}%n%f%u@%U%F{blue}%m%f%u:%F{yellow}%~%f
     %F{green}→%f "
    RPROMPT="%F{red}▂%f%F{yellow}▄%f%F{green}▆%f%F{cyan}█%f%F{blue}▆%f%F{magenta}▄%f%F{white}▂%f"
    [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
    '';
  };

  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellAliases = {
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
