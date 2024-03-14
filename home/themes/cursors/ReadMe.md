
This is something i was testing to make my own packages

{ pkgs, ... }:


# this is the almighty fn that is used in nix-os to build packages
pkgs.stdenv.mkDerivation {

  name = "bibata-mocha"; # it doesn't matter what you name

  src = ./Bibata-Mocha;
  /*
  src = pkgs.fetchurl {
    url = "<has to be a package like stuff with makefile or buildphase>";
    sha256 = "<its sha256 got by nix-prefetch-url 'above url'>";
  };
  # */

  # dontUnpack = true;

  dontWrapGzip = true;

  /*
  installPhase = ''
  echo "-------->>>>>>> Installing Cursor ${myName} <<<<<<<---------"
  echo "-------->>>>>>> src:$src out:$out <<<<<<<---------"
  mkdir -p $out/share/icons
  echo "out = $out"
  cp -r $src/* $out/share/icons/
  '';
  # */

  # /*
  installPhase = ''
  echo "-------->>>>>>> Installing Cursor <<<<<<<---------"
  echo "-------->>>>>>> src:$src out:$out <<<<<<<---------"
  mkdir -p $out/.icons
  cp -r $src $out/.icons/
  echo "-------->>>>>>> DONE <<<<<<<---------"
  '';
  # */
  # mkdir -p $out/share/icons
  # cp -r $src $out/share/icons/
  # mkdir -p $out
  # cp -r $src/Bibata-* -d $out
  # cp -r $src/Bibata-Mocha/ $out/share/icons/Bibata-Mocha
}


/*
  # Copy the cursor folder
  mv Bibata-* ~/.icons/                 # Install to local users
  sudo mv Bibata-* /usr/share/icons/    # Install to all users

  # Copy Bibata-Mocha folder
  home.file."~/.icons/Bibata-Mocha".source = ./Bibata-Mocha;
  home.file."/usr/share/icons/Bibata-Mocha".source = ./Bibata-Mocha;
# */

