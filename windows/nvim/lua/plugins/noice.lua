return {
  "folke/noice.nvim",
  opts = {

    routes = {
      {
        filter = {
          event = "notify",
          find = "Toggling hidden files",
        },
        opts = { skip = true },
      },
      {
        filter = {
          event = "msg_show",
          kind = "",
          find = '"~',
        },
        opts = { skip = true },
      },
    },

    lsp = {
      override = {
        ["vim.lsp.util.convert_input_to_markdown_lines"] = false,
        ["vim.lsp.util.stylize_markdown"] = false,
        ["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
      },
    },

    presets = { bottom_search = true, command_palette = false, lsp_doc_border = true },
  },
}
