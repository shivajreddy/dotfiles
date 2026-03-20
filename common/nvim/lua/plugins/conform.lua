-- for formatting python files
return {
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
