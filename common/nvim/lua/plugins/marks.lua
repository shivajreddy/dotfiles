return {
  "chentoast/marks.nvim",
  event = "VeryLazy",
  opts = {
    -- Show marks a-z, A-Z in the signcolumn
    default_mappings = true,
    -- Refresh marks when text changes
    refresh_interval = 250,
    -- Mark priorities (higher = more visible when overlapping)
    sign_priority = { lower = 10, upper = 15, builtin = 8, bookmark = 20 },
    -- Excluded filetypes
    excluded_filetypes = {
      "neo-tree",
      "toggleterm",
      "snacks_terminal",
      "TelescopePrompt",
      "dashboard",
      "lazy",
      "mason",
    },
    -- Bookmark icons (not marks a-z)
    bookmark_0 = {
      sign = "⚑",
      virt_text = "bookmark",
    },
    mappings = {
      delete_buf = "dm<space>",    -- delete all marks in current buffer
      delete_line = "dm-",         -- delete all marks on current line
      delete = "dm",               -- delete a specific mark (waits for input)
    },
  },
}
