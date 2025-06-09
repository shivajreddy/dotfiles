return {
  "folke/snacks.nvim",
  opts = {
    picker = {
      sources = {
        explorer = {
          -- focus = "input",
          -- auto_close = true,
          -- layout = { layout = { position = "right" } },
          include = {
            ".env*",
            -- ".gitignore"
          },
          -- hidden = true,
          -- ignored = true,
        },
      },
    },
  },

  keys = {
    -- disable the keymap to grep files
    { "<leader>/", false },
  },
}
