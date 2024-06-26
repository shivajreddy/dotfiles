{ ... }:
{

  programs.git = {
    enable = true;
    userName = "shivajreddy";
    userEmail = "shivajreddy@outlook.com";
    extraConfig.credential.helper = "store";
    # Note: first time using git, when pushing to your account,
    # use username, for password paste the token from github.com 

    extraConfig = {
      http.postBuffer = "2147483648"; # 2 GB
    };

  };

  programs.lazygit.enable = true;

}
