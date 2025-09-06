return {
  -- Kanagawa
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

  --  catppuccin
  {
    "catppuccin/nvim",
    lazy = false,
    name = "catppuccin",
    opts = {
      flavour = "latte", -- latte, frappe, macchiato, mocha
      -- flavour = "mocha", -- latte, frappe, macchiato, mocha
      -- background = { -- :h background
      --   light = "latte",
      --   dark = "mocha",
      -- },
      -- transparent_background = true, -- disables setting the background color.
      show_end_of_buffer = false, -- shows the '~' characters after the end of buffers
      term_colors = false, -- sets terminal colors (e.g. `g:terminal_color_0`)
      dim_inactive = {
        enabled = false, -- dims the background color of inactive window
        shade = "dark",
        percentage = 0.85, -- percentage of the shade to apply to the inactive window
      },
      no_italic = false, -- Force no italic
      no_bold = false, -- Force no bold
      no_underline = false, -- Force no underline
      styles = { -- Handles the styles of general hi groups (see `:h highlight-args`):
        comments = { "italic" }, -- Change the style of comments
        conditionals = { "italic" },
        loops = {},
        functions = {},
        keywords = {},
        strings = {},
        variables = {},
        numbers = {},
        booleans = {},
        properties = {},
        types = {},
        operators = {},
        -- miscs = {}, -- Uncomment to turn off hard-coded styles
      },
      color_overrides = {
        mocha = {
          base = "#181825",
          -- base = "#11111b",
        },
      },
      custom_highlights = function()
        return {
          -- NeoTree
          -- NeoTreeNormal = { bg = "#11111b" },
          -- NeoTreeNormalNC = { bg = "#11111b" },
        }
      end,
      default_integrations = true,
      integrations = {
        aerial = true,
        alpha = true,
        cmp = true,
        dashboard = true,
        flash = true,
        gitsigns = true,
        headlines = true,
        illuminate = true,
        indent_blankline = { enabled = true },
        leap = true,
        lsp_trouble = true,
        mason = true,
        markdown = true,
        mini = {
          enabled = true,
          indentscope_color = "",
        },
        native_lsp = {
          enabled = true,
          underlines = {
            errors = { "undercurl" },
            hints = { "undercurl" },
            warnings = { "undercurl" },
            information = { "undercurl" },
          },
        },
        navic = { enabled = true, custom_bg = "lualine" },
        neotest = true,
        -- neotree = true,
        neotree = { enabled = true, active_bg = "#11111b" },
        -- neotree = { enabled = true, active_bg = "#181825" },
        noice = true,
        notify = true,
        nvimtree = true,
        semantic_tokens = true,
        telescope = true,
        treesitter = true,
        treesitter_context = true,
        which_key = true,
      },
    },
  },

  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "kanagawa",
      -- colorscheme = "catppuccin",
    },
  },
}
