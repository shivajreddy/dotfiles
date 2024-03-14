{ pkgs, ... }:


# this is the almighty fn that is used in nix-os to build packages
pkgs.stdenv.mkDerivation {

  name = "bibata-mocha"; # it doesn't matter what you name

  src = ./Bibata-Mocha;

  dontWrapGzip = true;

  installPhase = ''
  echo "-------->>>>>>> Installing Cursor <<<<<<<---------"
  echo "-------->>>>>>> src:$src out:$out <<<<<<<---------"
  cp -r $src $out/
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

  /*
  postInstall = ''
  echo "To link these icons to your home directory, run:"
  echo "-------->>>>>>> DONE <<<<<<<---------"
  '';
  */

  # install -dm 0755 $out/share/icons
  # echo "ln -sfn $out/share/icons ~/.icons/bibata-mocha"
  # mkdir -p $out/.icons

