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
        -- React / JS / TS — use project's .prettierrc
        javascript = { "prettierd", "prettier", stop_after_first = true },
        javascriptreact = { "prettierd", "prettier", stop_after_first = true },
        typescript = { "prettierd", "prettier", stop_after_first = true },
        typescriptreact = { "prettierd", "prettier", stop_after_first = true },
        css = { "prettierd", "prettier", stop_after_first = true },
        html = { "prettierd", "prettier", stop_after_first = true },
      },
      formatters = {
        prettierd = {
          -- Ensure prettierd respects the project-local .prettierrc
          env = {
            PRETTIERD_DEFAULT_CONFIG = vim.fn.expand("~/.prettierrc"),
          },
        },
      },
    },
  },
}
