return {
  {
    "rose-pine/neovim",
    name = "rose-pine",
    config = function()
      require("rose-pine").setup({
        variant = "auto", -- auto, main, moon, or dawn

        styles = {
          bold = true,
          italic = false,
          transparency = true, -- Must not set any color
        },
        -- in repo: https://github.dev/rose-pine/neovim, the file: rose-pine.lua
        -- contains all the highlight group names, under the variable `default_highlights`
        highlight_groups = {
          -- Normal = { bg = "#0e0a01" }, -- rosepine burnt background
          CursorLine = { bg = "#000000" }, -- #0e0a01 base  #121F2B none   #6e6a86
          ColorColumn = { bg = "#121F2B" }, -- #0e0a01
          SignColumn = { bg = "#31748f", fg = "#f6c177" },
          LineNr = { fg = "#413630" },
          CursorLineNr = { fg = "#908caa" }, -- rosepine subtle
        },
      })
    end,
  },

  -- {
  --   "metalelf0/base16-black-metal-scheme",
  --   name = "metalelf0",
  -- },

  {
    "LazyVim/LazyVim",
    opts = {
      -- colorscheme = "rose-pine",
      -- colorscheme = "brightburn",
      colorscheme = "shiva_metal",
      -- colorscheme = "base16-black-metal-marduk", -- venom nile  mayhem  marduk  immortal
    },
  },
}
