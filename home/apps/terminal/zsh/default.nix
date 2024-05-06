{pkgs, ...}: let


  # My Scripts
  zsh_scripts = pkgs.lib.readFile ./scripts/zsh_scripts.sh;

  # My shell aliases
  myAliases = {
    ls = "eza --icons -l -T -L=1";
    vi = "nvim";
    files = "nautilus";
    gaa = "git add .";
    gcmsg = "git commit -m";
    gst = "git status .";
    uxplay = "~/dotfiles/home/apps/uxplay/uxplay-script.bash";
    gitsave = "gaa && gcmsg '.' && ggpush";
  };
in {

  # copy the scripts folder to .config/zsh/
  xdg.configFile."zsh/scripts".source = ./scripts;

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
      WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
      ${zsh_scripts}
    '';
      # [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
  };


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
