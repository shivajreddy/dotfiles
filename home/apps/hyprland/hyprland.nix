{ config, lib, pkgs, ... }:

let 
  	# tomlFile = builtins.readFile (./. + "/starship.toml");
 	autostartConf = builtins.readFile (./. + "/autostart.conf");
	keybindsConf = builtins.readFile (./. + "/keybinds.conf");
 	decorationsConf = builtins.readFile (./. + "/decorations.conf");
	workspacesConf = builtins.readFile (./. + "/workspaces.conf");
in

{
	imports = [
	];
	wayland.windowManager.hyprland = {
		enable = true;
		plugins = [];
		settings = {};
		xwayland = { enable = true; };
		systemd.enable = true;
	        extraConfig = ''
		${autostartConf}
		${keybindsConf}
		${decorationsConf}
		${workspacesConf}
		'';
	};


/* Install pyprland using python3
    (pkgs.python3Packages.buildPythonPackage rec {
      pname = "pyprland";
      version = "1.4.1";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "sha256-JRxUn4uibkl9tyOe68YuHuJKwtJS//Pmi16el5gL9n8=";
      };
      format = "pyproject";
      propagatedBuildInputs = with pkgs; [
        python3Packages.setuptools
        python3Packages.poetry-core
        poetry
      ];
      doCheck = false;
    })
  ];
# */


 #  home.file.".config/hypr/pyprland.toml".source = ./pyprland.toml;


  home.file.".config/hypr/pyprland.json".text = ''
    {
      "pyprland": {
        "plugins": ["scratchpads"]
      },
      "scratchpads": {
        "term": {
		"command" : "kitty --class scratchpad",
		"class" : "scratchpad",
		"size" : "75% 60%",
		"max_size" : "1920px 100%",
        },
      }
    }
  '';
}

