{ pkgs, ... }:


# this is the almighty fn that is used in nix-os to build packages
pkgs.stdenv.mkDerivation {

  # WARNING: what is the diff b/w name and pname;

  # name = "bibata-mocha"; # it doesn't matter what you name
  pname = "bibata-mocha"; 
  version = "1.0.0";

  src = ./Bibata-Mocha;

  dontWrapGzip = true;

  installPhase = ''
  echo "-------->>>>>>> Installing Cursor <<<<<<<---------"
  echo "-------->>>>>>> src:$src out:$out <<<<<<<---------"
  mkdir -p $out/share/icons
  cp -rf $src/* $out/share/icons/Bibata-Mocha
  '';

}


/*
  # Copy the cursor folder
  mv Bibata-* ~/.icons/                 # Install to local users
  sudo mv Bibata-* /usr/share/icons/    # Install to all users

  install -dm 0755 $out/share/icons
  echo "ln -sfn $out/share/icons ~/.icons/bibata-mocha"
  mkdir -p $out/.icons

  postInstall = ''
  echo "To link these icons to your home directory, run:"
  echo "-------->>>>>>> DONE <<<<<<<---------"
  '';

# */

