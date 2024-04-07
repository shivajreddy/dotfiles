{pkgs, ...}: let

  # My Scripts
  aliases_script = pkgs.lib.readFile ./aliases.sh;

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
    rebuild = "home-manager switch --flake /home/shiva/dotfiles";
    savedots = "cd ~/dotfiles && gaa && gcmsg '.' && ggpush";
    uxplay = "~/dotfiles/home/apps/uxplay/uxplay-script.bash";
    todo = "cd ~/todo && vi todo.md"
  };
in {
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    shellAliases = myAliases;
    initExtra = ''
      zstyle ':completion:*' matcher-list "" 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
      PROMPT="%U%F{magenta}%n%f%u@%U%F{blue}%m%f%u:%F{yellow}%~%f
       %F{green}→%f "
      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
      ${aliases_script}
    '';
  };


  /*
    programs.bash = {
      enable = true;
      enableCompletion = true;
      shellAliases = myAliases;
    };
  #
  */

  home.packages = with pkgs; [
    direnv
    nix-direnv
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

}
