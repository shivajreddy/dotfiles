return {
  "lukas-reineke/indent-blankline.nvim",
  event = "LazyFile",
  opts = {
    scope = { enabled = false },
  },
  keys = {
    -- disable the keymap to toggle indentation guides
    { "<leader>ug", false },
  },
}
