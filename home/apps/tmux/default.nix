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

    extraConfig = ''
      ${main_tmux_conf}
    '';

    # installing tmux plugins through nix, since TPM wont work
    plugins = with pkgs; [
      tmuxPlugins.better-mouse-mode
      tmuxPlugins.vim-tmux-navigator

      # tmuxplugins as of this day is soo many months old,
      # so i made the custom tmux-plugin from the catppuccin-tmux repo on 2024-04-24
      {
        plugin = (
          mkTmuxPlugin {
            pluginName = "catppuccin";
            version = "unstable-2024";
            src = pkgs.fetchFromGitHub {
              owner = "catppuccin";
              repo = "tmux";
              rev = "a556353d60833367b13739e660d4057a96f2f4fe"; # 2024-04-24
              hash = "sha256-i5rnMnkFGOWeRi38euttei/fVIxlrV6dQxemAM+LV0A=";
            };
          }
        );
        extraConfig = ''
          set -g @catppuccin_flavour  "mocha"

          set -g @catppuccin_status_background "default"

          set -g @catppuccin_window_number_position "right"

          set -g @catppuccin_window_left_separator " "
          set -g @catppuccin_window_right_separator ""
          # set -g @catppuccin_window_left_separator "#[fg=#ffffff,bg=#ffffff,bold,underscore,italics] "
          # set -g @catppuccin_window_right_separator "#[fg=#ff0000,bg=#ff0000,bold,underscore,italics]"

          set -g @catppuccin_window_default_fill "none"                        # number all none
          set -g @catppuccin_window_current_fill "all" # number all none
          # set -g @catppuccin_window_status_default "off"
          set -g @catppuccin_window_current_color "#f5c2e7"
          set -g @catppuccin_window_current_text_color "#11111b"

          # set -g @catppuccin_window_default_text " #W 󰇙 #{b:pane_current_path} "
          # set -g @catppuccin_window_current_text "#[fg=#11111b,bold] #W 󰇙 #{b:pane_current_path} #{?window_zoomed_flag,  ,}"

          # set -g @catppuccin_window_default_text "#W·#{b:pane_current_path} "
          set -g @catppuccin_window_default_text "#W·#{s|^$HOME|~|{b:pane_current_path}} "
          set -g @catppuccin_window_current_text "#W·#{s|^$HOME|~|{b:pane_current_path}} "
          # set -g @catppuccin_window_current_text "#[fg=#11111b,bold]#W·#{b:pane_current_path} #{?window_zoomed_flag,  ,}"
          # set -g @catppuccin_window_default_text "#{s|^$HOME|~|:pane_current_path} #W·#{b:pane_current_path} "
          # set -g @catppuccin_window_current_text "#[fg=#11111b,bold]#W·#{b:pane_current_path} #{?window_zoomed_flag,  ,}"

          # set -g @catppuccin_window_default_text " #W"
          # set -g @catppuccin_window_current_text "#[fg=#11111b,bold] #W#{?window_zoomed_flag,  ,}"

          set -g @catppuccin_window_middle_separator " "
          # set -g @catppuccin_window_middle_separator " █"

          set -g @catppuccin_status_modules_right "session date_time"
          set -g @catppuccin_status_modules_left ""
          set -g @catppuccin_status_left_separator  " "
          set -g @catppuccin_status_right_separator " "
          set -g @catppuccin_status_right_separator_inverse "no"
          set -g @catppuccin_status_fill "icon"
          set -g @catppuccin_status_connect_separator "no"
          set -g @catppuccin_directory_text "#{b:pane_current_path}"
          set -g @catppuccin_date_time_text "%I:%M %p"
        '';
      }

    ];

  };
}
