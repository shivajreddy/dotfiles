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

      {
        plugin = tmuxPlugins.rose-pine;
        extraConfig = ''
          set -g @rose_pine_variant 'main' # Options are 'main', 'moon' or 'dawn'

          set -g @rose_pine_host 'on' # Enables hostname in the status bar
          set -g @rose_pine_date_time "" # It accepts the date UNIX command format (man date for info)
          set -g @rose_pine_user 'on' # Turn on the username component in the statusbar
          set -g @rose_pine_directory 'on' # Turn on the current folder component in the status bar
          set -g @rose_pine_bar_bg_disable 'on' # Disables background color, for transparent terminal emulators
          set -g @rose_pine_bar_bg_disabled_color_option 'default'

          # Example values for these can be:
          set -g @rose_pine_left_separator "" # The strings to use as separators are 1-space padded
          set -g @rose_pine_right_separator "" # Accepts both normal chars & nerdfont icons
          set -g @rose_pine_field_separator "" # Again, 1-space padding, it updates with prefix + I
          set -g @rose_pine_window_separator "" # Replaces the default `:` between the window number and name

          # These are not padded
          set -g @rose_pine_session_icon ' ' # Changes the default icon to the left of the session name
          set -g @rose_pine_current_window_icon ' ' # Changes the default icon to the left of the active window name
          set -g @rose_pine_folder_icon ' ' # Changes the default icon to the left of the current directory folder
          set -g @rose_pine_username_icon ' ' # Changes the default icon to the right of the hostname
          set -g @rose_pine_hostname_icon '󰒋 ' # Changes the default icon to the right of the hostname
          set -g @rose_pine_date_time_icon '󰃰 ' # Changes the default icon to the right of the date module
          set -g @rose_pine_window_status_separator "" # Changes the default icon that appears between window names

          # Very beta and specific opt-in settings, tested on v3.2a, look at issue #10
          set -g @rose_pine_prioritize_windows 'on' # Disables the right side functionality in a certain window count / terminal width
          set -g @rose_pine_width_to_hide '80' # Specify a terminal width to toggle off most of the right side functionality
          set -g @rose_pine_window_count '5' # Specify a number of windows, if there are more than the number, do the same as width_to_hide
        '';
      }
      /*
          set -g @rose_pine_only_windows 'on' # Leaves only the window module, for max focus and space
          set -g @rose_pine_disable_active_window_menu 'on' # Disables the menu that shows the active window on the left

          set -g @rose_pine_default_window_behavior 'on' # Forces tmux default window list behaviour
          set -g @rose_pine_show_current_program 'on' # Forces tmux to show the current running program as window name
          set -g @rose_pine_show_pane_directory 'off' # Forces tmux to show the current directory as window name
      */

      /* Catppuccin Theme
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

          set -g @catppuccin_window_default_fill "all"              # number all none
          set -g @catppuccin_window_current_fill "all"              # number all none
          # set -g @catppuccin_window_status_default "off"

          set -g @catppuccin_window_default_text "#W·#{b:pane_current_path} "
          set -g @catppuccin_window_default_color "#181825"         # this is the background color of default-window
          set -g @catppuccin_window_default_background "#585b70"    # this is the text of default-window

          set -g @catppuccin_window_current_text "#[bold]#W·#{b:pane_current_path} #[bold]#{?window_zoomed_flag, ,}"
          set -g @catppuccin_window_current_color "#6c7086"         # this is the background color of current-window
          set -g @catppuccin_window_current_background "#11111b"    # this is the text color of current-window
          # set -g @catppuccin_window_current_color "#f5c2e7"   # this is the current-window-background-color

          # set -g @catppuccin_window_current_text "#[fg=#11111b,bold]#W·#{b:pane_current_path} #{?window_zoomed_flag,  ,}"


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
      # */

    ];

  };
}
