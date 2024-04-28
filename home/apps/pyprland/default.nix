{ lib, pkgs }:

pkgs.python3Packages.buildPythonApplication rec {
  pname = "pyprland";
  version = "2.0.9";
  format = "pyproject";

  disabled = pkgs.python3Packages.pythonOlder "3.10";

  src = pkgs.fetchFromGitHub {
    owner = "hyprland-community";
    repo = "pyprland";
    rev = "refs/tags/${version}";
    hash = "sha256-dfE4KQguLp9DEWOuCtNDw8TA3sK9vEqU4VqAVlVaUvw=";
  };

}
