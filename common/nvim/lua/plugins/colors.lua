--[[
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
          italic = true,
          transparency = true, -- Must not set any color
        },
        -- in repo: https://github.dev/rose-pine/neovim, the file: rose-pine.lua
        -- contains all the highlight group names, under the variable `default_highlights`
        highlight_groups = {
          -- Normal = { bg = "#0e0a01" }, -- rosepine burnt background
          CursorLine = { bg = "none" }, -- #0e0a01 base  #121F2B none
          ColorColumn = { bg = "#121F2B" }, -- #0e0a01
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
    },
  },
}
]]

return {
  {
    "metalelf0/base16-black-metal-scheme",
    name = "metalelf0",
    config = function()
      require().setup({
        variant = "auto", -- auto, main, moon, or dawn

        styles = {
          bold = true,
          italic = true,
          transparency = true, -- Must not set any color
        },
        -- in repo: https://github.dev/rose-pine/neovim, the file: rose-pine.lua
        -- contains all the highlight group names, under the variable `default_highlights`
        highlight_groups = {
          -- Normal = { bg = "#0e0a01" }, -- rosepine burnt background
          CursorLine = { bg = "none" }, -- #0e0a01 base  #121F2B none
          ColorColumn = { bg = "#121F2B" }, -- #0e0a01
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
      colorscheme = "base16-black-metal-venom",
    },
  },
}
