{pkgs, ...}: let

  # My Scripts
  # aliases_script = pkgs.lib.readFile ./aliases.sh;

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
    uxplay = "~/dotfiles/home/apps/uxplay/uxplay-script.bash";
    gitsave = "gaa && gcmsg '.' && ggpush";

    osbuild = "${pkgs.lib.getBin pkgs.bash}/bin/bash ${/absolute/path/to/your/dotfiles}/scripts/osbuild.sh";
    savedots = "${pkgs.lib.getBin pkgs.bash}/bin/bash ${/absolute/path/to/your/dotfiles}/scripts/savedots.sh";
    rebuild = "${pkgs.lib.getBin pkgs.bash}/bin/bash ${HOME}/.config/zsh/scripts/rebuild.sh";
    aliases = "${pkgs.lib.getBin pkgs.bash}/bin/bash ${/absolute/path/to/your/dotfiles}/scripts/aliases.sh";

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
      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
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
