{...}:

let
    user_chrome_css = builtins.readFile ("./userChrome.css");
in 

{

  programs.firefox = {
    enable = true;
    userChrome = ''
    ${user_chrome_css}
    '';
  };

}
