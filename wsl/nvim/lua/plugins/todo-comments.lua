return {
  "folke/todo-comments.nvim",
  opts = {
    keywords = {
      -- Keep existing keywords
      FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "ISSUE" } },
      TODO = { icon = " ", color = "info" },
      HACK = { icon = " ", color = "warning" },
      WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
      PERF = { icon = " ", color = "default", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
      NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
      TEST = { icon = "⏲ ", color = "test", alt = { "TESTING", "PASSED", "FAILED" } },
      -- Add your custom keyword here
      CUSTOM = {
        -- icon = " ",
        icon = " ",
        color = "error", -- Can be "error", "warning", "info", "hint", "default", or a color like "#ff0000"
        alt = { "C.E", "ERROR" }, -- Alternative keywords
        -- Highlight group for the keyword. You can also specify a color using #RRGGBB
      },
    },
    -- Optionally customize the colors
    colors = {
      error = { "DiagnosticError", "ErrorMsg", "#DC2626" },
      warning = { "DiagnosticWarn", "WarningMsg", "#FBBF24" },
      info = { "DiagnosticInfo", "#2563EB" },
      hint = { "DiagnosticHint", "#10B981" },
      default = { "Identifier", "#7C3AED" },
      test = { "Identifier", "#FF00FF" },
    },
  },
}
