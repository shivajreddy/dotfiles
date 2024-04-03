{
  pkgs,
  lib,
  ...
}: {
  imports = [
  ];

  config = {
    extraPackages = with pkgs; [
      lua-language-server
      stylua
      ripgrep
    ];
  };
}
