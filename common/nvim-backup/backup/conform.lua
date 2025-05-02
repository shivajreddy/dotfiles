return {
  "stevearc/conform.nvim",
  optional = true,
  opts = {
    formatters_by_ft = {
      python = { "black" },
      markdown = { "prettier" },
    },
    formatters = {
      prettier = {
        prepend_args = {
          "--print-width",
          "80",
          "--prose-wrap",
          "always",
        },
      },
    },
  },
}
