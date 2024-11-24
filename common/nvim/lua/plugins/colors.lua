return {
  {
    "erikbackman/brightburn.vim",
    name = "brightburn",
  },
  {
    "rose-pine/neovim",
    name = "rose-pine",
    config = function()
      require("rose-pine").setup({
        variant = "auto", -- auto, main, moon, or dawn
        styles = {
          bold = true,
          italic = false,
          transparency = true,
        },
        groups = {
          -- h1 = "love",
        },
        -- in repo: https://github.dev/rose-pine/neovim, the file: rose-pine.lua
        -- contains all the highlight group names, under the variable `default_highlights`
        highlight_groups = {
          Normal = { bg = "#0e0a01" }, -- rosepine burnt background
          CursorLine = { bg = "base" },
          ColorColumn = { bg = "#0e0a01" },
          SignColumn = { bg = "#31748f", fg = "#f6c177" },
          LineNr = { fg = "#413630" },
          CursorLineNr = { fg = "#908caa" }, -- rosepine subtle
        },
      })
    end,
  },

  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "rose-pine",
      -- colorscheme = "rose-pine-moon",
      -- colorscheme = "catppuccin",
      -- colorscheme = "catppuccin-mocha",
      -- colorscheme = "brightburn",
    },
  },
}
