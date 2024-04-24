{ pkgs, lib, ... }:
let
  main_tmux_conf = builtins.readFile ./tmux.conf;


  rtpPath = "share/tmux-plugins";

  addRtp = path: rtpFilePath: attrs: derivation:
    derivation // { rtp = "${derivation}/${path}/${rtpFilePath}"; } // {
      overrideAttrs = f: mkTmuxPlugin (attrs // f attrs);
    };

  mkTmuxPlugin = a@{
    pluginName,
    rtpFilePath ? (builtins.replaceStrings ["-"] ["_"] pluginName) + ".tmux",
    namePrefix ? "tmuxplugin-",
    src,
    unpackPhase ? "",
    configurePhase ? ":",
    buildPhase ? ":",
    addonInfo ? null,
    preInstall ? "",
    postInstall ? "",
    path ? lib.getName pluginName,
    ...
  }:
    if lib.hasAttr "dependencies" a then
      throw "dependencies attribute is obselete. see NixOS/nixpkgs#118034" # added 2021-04-01
    else addRtp "${rtpPath}/${path}" rtpFilePath a (pkgs.stdenv.mkDerivation (a // {
      pname = namePrefix + pluginName;

      inherit pluginName unpackPhase configurePhase buildPhase addonInfo preInstall postInstall;

      installPhase = ''
        runHook preInstall

        target=$out/${rtpPath}/${path}
        mkdir -p $out/${rtpPath}
        cp -r . $target
        if [ -n "$addonInfo" ]; then
          echo "$addonInfo" > $target/addon-info.json
        fi

        runHook postInstall
      '';
    }));


in 
{


  imports = [];

  programs.tmux = {
    enable = true;

    extraConfig = ''
      ${main_tmux_conf}
    '';

    # installing tmux plugins through nix, since TPM wont work
    plugins = with pkgs; [
      tmuxPlugins.better-mouse-mode
      tmuxPlugins.vim-tmux-navigator

      {
      plugin = (
        mkTmuxPlugin {
          pluginName = "catppuccin";
          version = "unstable-2024";
          src = pkgs.fetchFromGitHub {
            owner = "catppuccin";
              # "sha": "a556353d60833367b13739e660d4057a96f2f4fe",
            repo = "tmux";
            rev = "a556353d60833367b13739e660d4057a96f2f4fe";
                        # got:    sha256-i5rnMnkFGOWeRi38euttei/fVIxlrV6dQxemAM+LV0A=

            hash = "sha256-i5rnMnkFGOWeRi38euttei/fVIxlrV6dQxemAM+LV0A=";
          };
        }
      );
        extraConfig = ''
        set -g @catppuccin_flavour  "mocha"

        # set -g @catppuccin_status_default "off"
        set -g @catppuccin_status_background "default"
        '';
      }

    ];

  };
}
