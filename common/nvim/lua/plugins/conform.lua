-- for formatting python files
return {
  -- Disable nvim-ts-autotag (causes cursor issues and errors when typing >)
  { "windwp/nvim-ts-autotag", enabled = false },
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        python = { "black" }, -- or "ruff_format", "yapf", "autopep8"
        json = { "prettierd", "prettier", stop_after_first = true },
        jsonc = { "prettierd" },
      },
    },
  },
}
