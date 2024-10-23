-- rose useful link
-- https://www.reddit.com/r/neovim/comments/19f7s7e/changing_default_ros%C3%A9_pine_colours/
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
          h1 = "love",
        },
        -- in repo: https://github.dev/rose-pine/neovim, the file: rose-pine.lua
        -- contains all the highlight group names, under the variable `default_highlights`
        highlight_groups = {
          -- Normal = { bg = "#061111" },
          Normal = { bg = "#0e0a01" }, -- rosepine burnt background
          -- ["String"] = { fg = "#27d653" },
          -- ["SignColumn"] = { bg = "#31748f", fg = "#f6c177" },
          -- ["SignColumn"] = { bg = "#31748f" },
          SignColumn = { bg = "#31748f", fg = "#f6c177" },
          -- CursorLine = { bg = "#31748f", fg = "#f6c177" },
          CursorLine = { bg = "NONE" },
        },
      })
    end,
  },

  --  catppuccin
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    opts = {
      no_italic = true,
      term_colors = true,
      transparent_background = false,
      styles = {
        comments = {},
        conditionals = {},
        loops = {},
        functions = {},
        keywords = {},
        strings = {},
        variables = {},
        numbers = {},
        booleans = {},
        properties = {},
        types = {},
      },
      color_overrides = {
        mocha = {
          base = "#0e0a01",
          mantle = "#0e0a01",
          crust = "#0e0a01",
        },
      },
      highlight_overrides = {
        all = function(_)
          return {
            CursorLine = { bg = "NONE" }, -- Disable cursor line background
          }
        end,
      },
      integrations = {
        telescope = {
          enabled = true,
          style = "nvchad",
        },
        dropbar = {
          enabled = true,
          color_mode = true,
        },
      },
    },
  },

  {
    "LazyVim/LazyVim",
    opts = {
      -- colorscheme = "rose-pine",
      -- colorscheme = "rose-pine-moon",
      colorscheme = "catppuccin",
      -- colorscheme = "catppuccin-mocha",
      -- colorscheme = "brightburn",
    },
  },
}
