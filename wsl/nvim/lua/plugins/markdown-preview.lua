return {
  -- Option 1: markdown-preview.nvim for live browser preview
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreview", "MarkdownPreviewStop" },
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
    keys = {
      {
        "<leader>cp",
        ft = "markdown",
        "<cmd>MarkdownPreview<cr>",
        desc = "Preview Markdown",
      },
    },
    config = function()
      vim.g.mkdp_auto_start = 0
      vim.g.mkdp_auto_close = 1
      vim.g.mkdp_refresh_slow = 0
      vim.g.mkdp_browser = "" -- Use default browser
      vim.g.mkdp_preview_options = {
        disable_sync_scroll = 0,
        disable_filename = 0,
      }
    end,
  },
}
