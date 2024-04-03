{
  pkgs,
  helpers,
  ...
}: {
  plugins = {
    neo-tree = {enable = true;};
    flash = {
      enable = true;
    };
  };
}
