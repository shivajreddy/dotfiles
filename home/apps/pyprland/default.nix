# { lib, pkgs }:
#
# pkgs.python3.pkgs.buildPythonApplication rec {
#   pname = "pyprland";
#   version = "2.0.9";
#   format = "pyproject";
#
#   # disabled = pkgs.python3Packages.pkgs.pythonOlder "3.10";
#
#   src = pkgs.fetchFromGitHub {
#     owner = "hyprland-community";
#     repo = "pyprland";
#     rev = "refs/tags/${version}";
#     hash = "sha256-dfE4KQguLp9DEWOuCtNDw8TA3sK9vEqU4VqAVlVaUvw=";
#   };
#
# }

{ lib, pkgs }:

pkgs.python3.pkgs.buildPythonApplication rec {
  pname = "my-python-package";
  version = "1.0.0";

  src = ./.;

  propagatedBuildInputs = with pkgs.python3.pkgs; [
    requests
    # Add other Python dependencies here
  ];

  # Optional: Specify the main entry point for your application
  # makeWrapperArgs = [
  #   "--set" "PYTHONPATH" "${lib.concatStringsSep ":" (map (py: "${py}/${py.sitePackages}") propagatedBuildInputs)}"
  # ];

  meta = with lib; {
    description = "My custom Python package";
    homepage = "https://example.com/my-python-package";
    license = licenses.mit;
    maintainers = [ maintainers.myUsername ];
  };
}
