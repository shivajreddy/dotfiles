
{...}:

let
    user_chrome_css = builtins.readFile (./userChrome.css);
in 

{

  programs.vivaldi = {
    enable = true;
  };

}