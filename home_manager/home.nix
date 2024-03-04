{ config, pkgs, ... }:

{
  home.username = "shiva";
  home.homeDirectory = "/home/shiva";

  home.stateVersion = "23.11"; # DONT CHANGE THIS

  home.packages = [
    # pkgs.hello
  ];

  home.file = {
    "mydir2/test.vim".text = ''
      this is a test file that is created using home-manager
    '';
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
}
