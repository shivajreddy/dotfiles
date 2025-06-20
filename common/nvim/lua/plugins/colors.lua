return {
  {
    "rebelot/kanagawa.nvim",
    name = "kanagawa",
    config = function()
      -- Default options:
      require("kanagawa").setup({
        compile = false, -- enable compiling the colorscheme
        undercurl = true, -- enable undercurls
        commentStyle = { italic = false },
        functionStyle = {},
        keywordStyle = { italic = false },
        statementStyle = { bold = true },
        typeStyle = {},
        transparent = true, -- do not set background color
        dimInactive = false, -- dim inactive window `:h hl-NormalNC`
        terminalColors = true, -- define vim.g.terminal_color_{0,17}
        colors = { -- add/modify theme and palette colors
          palette = {},
          theme = {
            wave = {},
            lotus = {},
            dragon = {},
            all = {
              ui = {
                bg_gutter = "none",
              },
            },
          },
        },
        overrides = function(colors) -- add/modify highlights
          return {
            -- Neovim's color groups
            LineNr = { bg = "#12120F", fg = "#303446" }, -- #1D1C19 Remove bg for transparent
            CursorLineNr = { fg = "#737c73" }, --bg = "#12120F",  #303446 --Remove bg for transparent
            CursorLine = { bg = "#181616" }, -- 0D0C0C 252329 12120F 0e0a01 base 121F2B none 6e6a86
            -- SignColumn = { bg = "#ffffff", fg = "#1f1f1f" },
            Whitespace = { fg = "#26262C" }, --  1f1f28 181921 Add this line to customize the Whitespace highlight group
            WinSeparator = { fg = "#6e6a86", bg = "NONE", bold = true }, -- Customize separator color

            -- Other plugins color groups
            SnacksPickerList = { bg = "NONE", fg = "#737c73" },
            SnacksPicker = { bg = "NONE", fg = "#737c73" },
            SnacksPickerListCursorLine = { fg = "#0d0c0c", bg = "#737c73" },
            SnacksPickerTree = { fg = "#737c73", bg = "NONE" },
          }
        end,
        theme = "wave", -- wave dragon lotus
        background = { -- map the value of 'background' option to a theme
          dark = "wave", -- try "dragon" !
          light = "lotus",
        },
      })
    end,
  },
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "kanagawa",
    },
  },
}
