return {
  "folke/snacks.nvim",
  ---@class snacks.indent.Config
  ---@field enabled? boolean
  opts = {
    indent = {
      enabled = false, --default is true, disable if using 'indent-blankline' plugin
    },
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
