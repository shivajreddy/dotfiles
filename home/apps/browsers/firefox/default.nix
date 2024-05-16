{...}:

let
    user_chrome_css = builtins.readFile (./userChrome.css);
in 

{

  programs.firefox = {
    enable = true;
    profiles."shiva" = {
      userChrome = ''
      ${user_chrome_css}
      '';
    };
  };

}
