return {
  {
    "akinsho/toggleterm.nvim",
    version = "*",
    config = function()
      require("toggleterm").setup({
        size = 20,
        open_mapping = false, -- Disable automatic mapping
        hide_numbers = true,
        shade_terminals = true,
        shading_factor = 2,
        direction = "float",
        float_opts = {
          border = "curved",
          winblend = 8,
          width = function()
            return math.floor(vim.o.columns * 0.9)
          end,
          height = function()
            return math.floor(vim.o.lines * 0.9)
          end,
        },
      })

      -- Set keymap for normal mode
      -- vim.keymap.set("n", "<F11>", function()
      --   require("toggleterm").toggle()
      -- end, { desc = "Toggle Terminal", noremap = true, silent = true })

      -- Set keymap for terminal mode to close the terminal
      -- vim.keymap.set("t", "<F11>", function()
      --   require("toggleterm").toggle()
      -- end, { desc = "Toggle Terminal", noremap = true, silent = true })
    end,
  },
}
