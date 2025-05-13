return {
  "folke/snacks.nvim",
  opts = {
    explorer = {
      -- layout = { layout = { position = "right" } },
      hidden = true,
      -- ignored = true,
    },
  },

  keys = {
    -- disable the keymap to grep files
    { "<leader>/", false },
  },
}
