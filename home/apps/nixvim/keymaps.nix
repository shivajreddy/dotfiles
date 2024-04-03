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
    {
      mode = "n";
      key = "<C-n>";
      action = "<cmd>Neotree toggle<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "Toggle Neotree tree";
      };
    }
  ];
}
