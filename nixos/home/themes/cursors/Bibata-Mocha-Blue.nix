{ pkgs, ... }:


# this is the almighty fn that is used in nix-os to build packages
pkgs.stdenv.mkDerivation {

  # Total 5 steps, 3 here, 2 on the gtk.nix file. 
  # After changing in all 5 places, reboot system.
  # and this should work for legacy apps too. if not 
  # then set run 'gnome-tweaks' on terminal and set it Appearance>Cursor
  # 1. set the name here
  pname = "bibata-mocha-blue";
  version = "1.0.0";

  # 2. use the proper directory here
  src = ./Bibata-Mocha-Blue;

  dontWrapGzip = true;

  # 3. set the proper directories in all the 4 commands below
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

