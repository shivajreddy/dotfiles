{...}:

let
    user_chrome_css = builtins.readFile (./userChrome.css);
in

{

  programs.firefox = {
    enable = true;
    profiles."Shiva" = {
      userChrome = ''
      ${user_chrome_css}
      '';
    };
  };

}
