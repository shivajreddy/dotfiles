{config, pkgs, ...}:
{

  programs.firefox = {
    enable = true;
    userChrome = builtins.readFile ("./userChrome.css");
  };

}
