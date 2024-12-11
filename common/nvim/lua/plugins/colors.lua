return {
  {
    "rose-pine/neovim",
    colorscheme = "simple-colon",
    config = function()
      require("rose-pine").setup({
        variant = "auto", -- auto, main, moon, or dawn

        palette = {
          main = {
            -- base = "#191724",
            -- surface = "#1f1d2e",
            -- overlay = "#26233a",
            -- muted = "#6e6a85",
            -- subtle = "#908caa",
            -- text = "#e0def4",
            -- love = "#f43841", --"#ffdd33",
            -- gold = "#cc8c3c", -- #95a99f,
            -- rose = "#ffdd33",
            pine = "#ffdd33",
            foam = "#e0def4",
            -- iris = "#ffdd33",
          },
        },

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
          -- String = { fg = "#73d936" },
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
<<<<<<< HEAD
      colorscheme = "simple-colon",
      -- colorscheme = "rose-pine",
=======
      colorscheme = "rose-pine",
>>>>>>> 697b67ff3383f9d564a0277b1c8cbd65482a938e
      -- colorscheme = "brightburn",
      -- colorscheme = "shiva_metal",
      -- colorscheme = "base16-black-metal-marduk", -- venom nile  mayhem  marduk  immortal
    },
  },
}
