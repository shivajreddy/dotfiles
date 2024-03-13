{ lib, ... }:
{
  imports = [];

  # Copy the cursor folder
  # mv Bibata-* ~/.icons/                 # Install to local users
  # sudo mv Bibata-* /usr/share/icons/    # Install to all users
  
  # Copy Bibata-Mocha folder
  home.file."~/.icons/Bibata-Mocha".source = ./Bibata-Mocha;
  home.file."/usr/share/icons/Bibata-Mocha".source = ./Bibata-Mocha;
# /home/shiva/.icons

}
