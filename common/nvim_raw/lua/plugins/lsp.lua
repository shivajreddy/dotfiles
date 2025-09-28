return {
  "hrsh7th/cmp-nvim-lsp",
  config = function()
    -- Native Neovim 0.11 LSP setup
    vim.api.nvim_create_autocmd("FileType", {
      pattern = { "c", "cpp" },
      callback = function()
        vim.lsp.start({
          name = "clangd",
          cmd = { "/opt/homebrew/bin/clangd" },
          root_dir = vim.fs.root(0, { ".clangd", ".git", "compile_commands.json" }) or vim.fn.getcwd(),
          capabilities = require("cmp_nvim_lsp").default_capabilities(),
        })
      end,
    })

    -- LSP keymaps
    vim.api.nvim_create_autocmd("LspAttach", {
      callback = function(args)
        local opts = { buffer = args.buf }
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
        vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
        vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
        vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
      end,
    })
  end,
}
