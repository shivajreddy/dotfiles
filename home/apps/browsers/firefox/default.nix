{ ... }:

let
    bottomtabs_css = builtins.readFile (./bottomtabs.css);
    arc_css = builtins.readFile (./arc.css);
    # user_chrome_css = builtins.readFile (./myUserChrome.css);
    # user_chrome_css = builtins.readFile (./arc.css);

    # user_js = builtins.readFile (./user.js);

    # Trying css-themes from https://firefoxcss-store.github.io/
in

{

  programs.firefox = {
    enable = true;
    profiles."shiva" = {

      # enable this by about:config > search 'userprof' > toolkit.legacyUserProfileCustomizations.stylesheets
      userChrome = ''
      ${arc_css}
      '';

      # extraConfig = ''
      # ${user_js}
      # '';

    };

  };

}
