{
  pkgs,
  helpers,
  ...
}: {
  plugins = {

    neo-tree.enable = true;

		tmux-navigator.enable = true;

    flash.enable = true;

    illuminate = {
      enable = true;
      delay = 200;
      largeFileOverrides.largeFileCutoff = 2000;
    };

    trouble = {
      enable = true;
      settings.use_diagnostic_signs = true;
    };

    todo-comments.enable = true;

  };
}
