-- For `plugins/markview.lua` users.
return {
  "OXY2DEV/markview.nvim",
  lazy = false,

  -- Completion for `blink.cmp`
  -- dependencies = { "saghen/blink.cmp" },

  keys = {
    { "<leader>md", "<cmd>Markview toggle<cr>", desc = "Markview Toggle" },
  },
}
