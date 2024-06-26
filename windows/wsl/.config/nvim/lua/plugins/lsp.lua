return {
  -- NOTE 5: set up the language's lsp config here
  {
    "neovim/nvim-lspconfig",
    ---@class PluginLspOpts
    opts = {
      inlay_hints = {},
      -- inlay_hints = { enabled = true }, -- this is deprecated, use this instead? inlay_hint.enable(),
      servers = {
        -- pyright will be automatically installed with mason and loaded with lspconfig
        -- pyright = {},
        -- clangd = {},
        -- cmake = {},
        rust_analyzer = {
          -- this is a good post: https://oneofone.dev/post/neovim-lsp-go-rust/
          tools = {},
          inlay_hints = {
            enabled = true,
            auto = true,
            show_parameter_hints = true,
          },
        },
      },
    },
  },
}
