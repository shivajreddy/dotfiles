
{ config, lib, pkgs, ... }:

let
tomlFileContent = builtins.readFile (./. + "/alacritty.toml");
in
{
  imports = [];

  programs.alacritty.enable = true;
  programs.alacritty.settings={
    env = {
      term = "xterm-245color";
    };
    colors = {
      draw_bold_text_with_bright_colors = true;
    };

    mouse = {
      hide_when_typing = true;
    };


    selection = {
      save_to_clipboard = true;
    };

    scrolling = {
      history = 10000;
      multiplier = 3;
# auto_scroll = true
    };

    window = {
      opacity = 1;
      dynamic_padding = false;

      padding.x = 6;
      padding.y = 6;

      decorations = "full";

      startup_mode = "Maximized";
    };

    class = {
      general = "Alacritty";
      instance = "Alacritty";
    };

    font = {
      size = 14;
      normal.family = "Berkeley Mono";
      normal.style = "regular";

      bold.family = "Berkeley Mono";
      bold.style = "bold";

      bold_italic.family = "Berkeley Mono";
      bold_italic.style = "bold italic";

      italic.family = "Berkeley Mono";
      italic.style = "italic";

      offset.x = 0;
      offset.y = 0;

      glyph_offset.x = 0;
      glyph_offset.y = 0;
    };

  };
}




