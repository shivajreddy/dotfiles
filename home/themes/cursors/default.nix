{ lib, pkgs, ... }:


# this is the almighty fn that is used in nix-os to build packages
pkgs.stdenv.mkDerivation {

  name = "bibata-mocha";  # it doesn't matter what you name

  src = pkgs.fetchurl {
    url = "";
    sah256 = "";
  };

  # Copy the cursor folder
  # mv Bibata-* ~/.icons/                 # Install to local users
  # sudo mv Bibata-* /usr/share/icons/    # Install to all users

  # Copy Bibata-Mocha folder
  home.file."~/.icons/Bibata-Mocha".source = ./Bibata-Mocha;
  home.file."/usr/share/icons/Bibata-Mocha".source = ./Bibata-Mocha;
  # /home/shiva/.icons

}
