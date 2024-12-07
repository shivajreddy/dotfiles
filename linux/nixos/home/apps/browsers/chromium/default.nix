{config, pkgs, ...}:

# let
#     chromeDesktopFile = pkgs.writeTextFile {
#     name = "chrome.desktop";
#     destination = "/share/applications/chrome.desktop";
#     text = ''
#       [Desktop Entry]
#       Type=Application
#       Name=Google Chrome
#       Exec=google-chrome-stable --ozone-platform=wayland
#       Icon=~/dotfiles/home/apps/browsers/chrome/chrome.png
#     '';
#   };
# in
{
  # programs.google-chrome.enable = true;
  programs.chromium.enable = true;

  # Create .desktop file for rofi to run with args
  # home.file.".local/share/applications/chrome.desktop".source = chromeDesktopFile;
}
