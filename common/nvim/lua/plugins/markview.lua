-- For `plugins/markview.lua` users.
return {
  "OXY2DEV/markview.nvim",
  lazy = false,

  opts = {
    preview = {
      enable = false,
      filetypes = {},
    },
  },

  keys = {
    { "<leader>md", "<cmd>Markview toggle<cr>", desc = "Markview Toggle" },
  },
}
