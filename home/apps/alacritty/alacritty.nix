
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
      normal.family = "berkeley mono nerd font";
      normal.style = "regular";

      bold.family = "berkeley mono nerd font";
      bold.style = "bold";

      bold_italic.family = "berkeley mono nerd font";
      bold_italic.style = "bold italic";

      italic.family = "berkeley mono nerd font";
      italic.style = "italic";

      offset.x = 0;
      offset.y = 0;

      glyph_offset.x = 0;
      glyph_offset.y = 0;
    };

  };
}




