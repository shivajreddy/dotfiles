{pkgs, inputs, ...}:

let
    # user_chrome_css = builtins.readFile (./userChrome.css);
    # user_chrome_css = builtins.readFile (./myUserChrome.css);
    user_chrome_css = builtins.readFile (./arc.css);

    user_js = builtins.readFile (./user.js);
in

{

  programs.firefox = {
    enable = true;
    profiles."shiva" = {
      userChrome = ''
      ${user_chrome_css}
      '';

      extraConfig = ''
      ${user_js}
      '';
    };

  };

}
