---@type LazySpec
return {
  "mikavilpas/yazi.nvim",
  enabled = true,
  event = "VeryLazy",
  log_level = vim.log.levels.DEBUG,
  bin = "/home/smpl/.cargo/bin/yazi",
  socket_dir = "/tmp/yazi-sockets",
  term = {
    type = "split", -- Try "split" instead of "float"
  },
  -- startup_delay = 500, -- Add a 500ms delay to give Yazi time to start
  -- args = { "--socket=/tmp/yazi-socket" }, -- Try specifying a socket path
  keys = {
    {
      -- "<leader>e",
      "<leader>z",
      "<cmd>Yazi<cr>",
      desc = "Open yazi at the current file",
    },
    {
      -- Open in the current working directory
      "<leader>cw",
      "<cmd>Yazi cwd<cr>",
      desc = "Open the file manager in nvim's working directory",
    },
    {
      -- NOTE: this requires a version of yazi that includes
      -- https://github.com/sxyazi/yazi/pull/1305 from 2024-07-18
      "<c-up>",
      "<cmd>Yazi toggle<cr>",
      desc = "Resume the last yazi session",
    },
  },
  ---@type YaziConfig
  opts = {
    -- if you want to open yazi instead of netrw, see below for more info
    open_for_directories = true,
    keymaps = {
      show_help = "<f1>",
    },
  },
}
