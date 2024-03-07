{ lib, ... }:
{
  imports = [];

  # Fonts
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-Bold.ttf".source = ./berkeley-mono/BerkeleyMono-Bold.ttf;
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-BoldItalic.ttf".source = ./berkeley-mono/BerkeleyMono-BoldItalic.ttf;
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-Italic.ttf".source = ./berkeley-mono/BerkeleyMono-Italic.ttf;
  home.file.".local/share/fonts/BerkeleyMono/BerkeleyMono-Regular.ttf".source = ./berkeley-mono/BerkeleyMono-Regular.ttf;

  home.file.".local/share/fonts/sf-san-francisco-pro".source = ./san-francisco-pro;

}
