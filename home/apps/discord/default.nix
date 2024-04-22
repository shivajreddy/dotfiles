{ config, inputs, pkgs, ... }:
let 
    discordDesktopFile = pkgs.writeTextFile {
    name = "discord.desktop";
    destination = "/share/applications/discord.desktop";
    text = ''
      [Desktop Entry]
      Type=Application
      Name=Discord
      Exec=webcord
      Icon=~/dotfiles/home/apps/discord/icon.png
    '';
  };
in
{

  xdg.configFile."WebCord/Themes/CatpuccinMocha".source = ./mocha.theme.css;


  home.file.".local/share/applications/chrome.desktop".source = chromeDesktopFile;

}
