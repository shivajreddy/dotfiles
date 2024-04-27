{ pkgs, lib, ... }:
let
  main_tmux_conf = builtins.readFile ./tmux.conf;


  # Got this from 
  # https://github.com/NixOS/nixpkgs/blob/2230a20f2b5a14f2db3d7f13a2dc3c22517e790b/pkgs/misc/tmux-plugins/default.nix
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

  /* Config the Nix way
    extraConfig = ''
      ${main_tmux_conf}
    '';

    # installing tmux plugins through nix, since TPM wont work
    plugins = with pkgs; [
      tmuxPlugins.better-mouse-mode
      tmuxPlugins.vim-tmux-navigator

      {
        plugin = tmuxPlugins.rose-pine;
        extraConfig = ''
          set -g @rose_pine_variant 'main' # Options are 'main', 'moon' or 'dawn'
        '';
      }

    ];
  */

  };
}
