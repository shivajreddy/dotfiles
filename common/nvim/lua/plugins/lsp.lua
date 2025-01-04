return {
  -- NOTE: 5 - set up the language's lsp config here
  {
    "neovim/nvim-lspconfig",
    ---@class PluginLspOpts
    opts = {
      inlay_hints = { enabled = false }, -- at starts sets them off
      servers = {
        --#region Clangd  Configuration
        clangd = {
          keys = {
            { "<leader>ch", "<cmd>ClangdSwitchSourceHeader<cr>", desc = "Switch Source/Header (C/C++)" },
          },
          root_dir = function(fname)
            return require("lspconfig.util").root_pattern(
              ".clang-format",
              ".git",
              "Makefile",
              "configure.ac",
              "configure.in",
              "config.h.in",
              "meson.build",
              "meson_options.txt",
              "build.ninja"
            )(fname) or require("lspconfig.util").root_pattern("compile_commands.json", "compile_flags.txt")(
              fname
            ) or require("lspconfig.util").find_git_ancestor(fname)
          end,
          capabilities = {
            offsetEncoding = { "utf-16" },
          },
          cmd = {
            "clangd",
            "--background-index",
            "--clang-tidy",
            "--header-insertion=iwyu",
            "--completion-style=detailed",
            "--function-arg-placeholders",
            "--fallback-style=llvm",
            "--enable-config",
            "--compile-commands-dir=.",
          },
          init_options = {
            usePlaceholders = true,
            completeUnimported = true,
            clangdFileStatus = true,
          },
        },
        --#endregion

        --#region Go Configuration
        -- https://github.com/ray-x/go.nvim
        gopls = {},
        --#endregion

        --#region Rust Configuration
        rust_analyzer = {
          -- this is a good post: https://oneofone.dev/post/neovim-lsp-go-rust/
          tools = {},
          inlay_hints = {
            enabled = true,
            auto = true,
            show_parameter_hints = true,
          },
        },
        --#endregion
      },

      setup = {
        -- Setup function to start LSP after configuration
        after = function()
          vim.schedule(function()
            vim.cmd("LspStart")
          end)
        end,
      },
    },
  },
}
