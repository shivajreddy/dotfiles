{ lib, pkgs, ... }:


# this is the almighty fn that is used in nix-os to build packages
pkgs.stdenv.mkDerivation {

  name = "bibata-mocha";  # it doesn't matter what you name

  src = pkgs.fetchurl {
    url = "https://github.com/shivajreddy/dotfiles/tree/main/home/themes/cursors/bibata-mocha.tar.gz";
    sha256 = "1r6qc4xw7528g1wzsdk5hmifz19rkjldh80s6pb1lh937ir2chsy";
  };

  dontUnpack = true;

  installPhase = ''
  echo "Installing Cursor ${name}"
  mkdir -p $out
  ${pkgs.unzip}/bin/unzip $src -d $out/
  '';

}


/*
  # Copy the cursor folder
  mv Bibata-* ~/.icons/                 # Install to local users
  sudo mv Bibata-* /usr/share/icons/    # Install to all users

  # Copy Bibata-Mocha folder
  home.file."~/.icons/Bibata-Mocha".source = ./Bibata-Mocha;
  home.file."/usr/share/icons/Bibata-Mocha".source = ./Bibata-Mocha;
# */

