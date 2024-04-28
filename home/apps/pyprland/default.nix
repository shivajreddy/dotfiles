{ pkgs }:

let
  pyprland = pkgs.python3Packages.buildPythonApplication rec {
    pname = "pyprland";
    version = "2.2.10"; # Update this to the desired version
    
    src = pkgs.fetchFromGitHub {
      owner = "hyprland-community";
      repo = "pyprland";
      rev = "v${version}"; # Use the appropriate Git tag or commit hash
      sha256 = "sha256-Lx9BYd/1kg/8C8Kwx+iumCJVaS5MyHmIK9ze7MmLrQk="; # Update this with the correct hash
    };

    format = "pyproject";
    
    nativeBuildInputs = with pkgs.python3Packages; [
      poetry-core
    ];

    propagatedBuildInputs = with pkgs.python3Packages; [
      aiofiles
    ];

    # Include any other necessary modifications from the official package definition

    meta = with pkgs.lib; {
      description = "An Hyprland plugin system";
      homepage = "https://github.com/hyprland-community/pyprland";
      license = licenses.mit;
      maintainers = with maintainers; [ iliayar ];
      platforms = platforms.linux;
    };
  };
in
{
  pyprland = pyprland;
}
