{ pkgs, ... }:


# this is the almighty fn that is used in nix-os to build packages
pkgs.stdenv.mkDerivation {

  pname = "bibata-mocha"; 
  version = "1.0.0";

  src = ./Bibata-Mocha-Blue;

  dontWrapGzip = true;

  installPhase = ''
  echo "-------->>>>>>> Installing Cursor <<<<<<<---------"
  echo "-------->>>>>>> src:$src out:$out <<<<<<<---------"

  echo "Copy for all users"
  mkdir -p $out/share/icons/Bibata-Mocha-Blue
  cp -rf $src/* $out/share/icons/Bibata-Mocha-Blue

  echo "Copy for current user"
  mkdir -p $out/.icons/Bibata-Mocha-Blue
  cp -rf $src/* $out/.icons/Bibata-Mocha-Blue
  '';

}


/* Copy the cursor folder
  mv Bibata-* ~/.icons/                 # Install to local users
  sudo mv Bibata-* /usr/share/icons/    # Install to all users
# */

