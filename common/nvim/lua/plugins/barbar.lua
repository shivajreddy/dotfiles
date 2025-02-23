return {
  "romgrk/barbar.nvim",
  lazy = false,
  dependencies = {
    "lewis6991/gitsigns.nvim", -- OPTIONAL: for git status
    "nvim-tree/nvim-web-devicons", -- OPTIONAL: for file icons
  },
  init = function()
    vim.g.barbar_auto_setup = false
  end,
  opts = {
    -- lazy.nvim will automatically call setup for you. put your options here, anything missing will use the default:
    animation = true,
    icons = {
      separator = { left = "▎", right = "" },
      -- separator = { left = "", right = "" },
      -- right_separator = { left = "", right = "" },
      -- separator_at_end = false,
    },
  },
  -- version = "^1.0.0", -- optional: only update when a new 1.x version is released
}
