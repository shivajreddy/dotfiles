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

    illuminate = {
      enable = true;
      delay = 200;
      largeFileOverrides.largeFileCutOff = 2000;
    };

    trouble = {
      enable = true;
      settings.use_diagnostic_signs = true;
    };

    todo-comments = {
      enable = true;
    };
  };
}
