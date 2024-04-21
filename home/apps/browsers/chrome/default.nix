{config, pkgs, ...}:

let
    chromeDesktopFile = pkgs.writeTextFile {
    name = "chrome.desktop";
    destination = "/share/applications/chrome.desktop";
    text = ''
      [Desktop Entry]
      Type=Application
      Name=google Chrome
      Exec=google-chrome-stable --ozone-platform=wayland
      Icon=chrome
    '';
  };
in
{
  programs.google-chrome.enable = true;

  # Create .desktop file for rofi to run with args
  home.file.".local/share/applications/chrome.desktop".source = chromeDesktopFile;
}
