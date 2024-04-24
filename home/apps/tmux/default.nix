{ pkgs, ... }:
let
  main_tmux_conf = builtins.readFile ./tmux.conf;
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

      /*
      {
        plugin = pkgs.fetchFromGitHub {
          owner = "catppuccin";
          repo = "tmux";
          rev = "v${version}";
          hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
        };
      }
      # */

    ];

  };
}
