{
  pkgs,
  helpers,
  ...
}: {
  keymaps = [
    {
      key = "<leader>j";
      mode = "n";
      action = "<cmd>lprev<CR>zz";
      options = {
        silent = true;
        noremap = true;
        desc = "Prev quickfix location";
      };
    }
  ];
}
