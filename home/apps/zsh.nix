{ pkgs, ... }:

let
  # My shell aliases
  myAliases = {
    ls = "eza --icons -l -T -L=1";
    vi = "nvim";
    files = "thunar";
    htop = "btm";
    gaa = "git add .";
    gcmsg = "git commit -m";
    gst = "git status .";
    ggpush = "git push -u origin main";
    ggpull = "git pull -u origin main";
    rebuild = "home-manager switch --flake /home/shiva/dotfiles"
  };
in
{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    shellAliases = myAliases;
    initExtra = ''
    PROMPT="%U%F{magenta}%n%f%u@%U%F{blue}%m%f%u:%F{yellow}%~%f
     %F{green}→%f "
    [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
    '';
  };

/*
  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellAliases = myAliases;
  };
# */

  home.packages = with pkgs; [
    direnv
    nix-direnv
    # disfetch lolcat cowsay onefetch
    # gnugrep gnused
    # bat eza bottom fd bc
    # direnv nix-direnv
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

}
