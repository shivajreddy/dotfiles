{pkgs, inputs, ...}:

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

     # extensions = with inputs.firefox-addons.packages."x86_64-linux"; [
    extensiosn = with pkgs.nur.repos.rycee.firefox-addons; [
    privacy-badger
          bitwarden
          block-origin
          ponsorblock
          arkreader
          ridactyl
          outube-shorts-block
        ];
  };

}
