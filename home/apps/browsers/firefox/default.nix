{pkgs, inputs, ...}:

let
    bottomtabs_css = builtins.readFile (./bottomtabs.css);
    # user_chrome_css = builtins.readFile (./myUserChrome.css);
    # user_chrome_css = builtins.readFile (./arc.css);

    # user_js = builtins.readFile (./user.js);

    # Trying css-themes from https://firefoxcss-store.github.io/
in

{

  programs.firefox = {
    enable = true;
    profiles."shiv" = {

      userChrome = ''
      ${bottomtabs_css}
      '';

      # extraConfig = ''
      # ${user_js}
      # '';
    };

  };

}
