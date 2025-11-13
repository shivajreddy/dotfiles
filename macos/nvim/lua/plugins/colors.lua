return {
  --Tsoding theme
  {
    "blazkowolf/gruber-darker.nvim",
    config = function()
      require("gruber-darker").setup({
        bold = false,
        italic = {
          strings = false,
          comments = false,
          operators = false,
          folds = true,
        },
        invert = {},
        undercurl = false,
        underline = false,
      })

      -- Apply custom highlights after setting the colorscheme
      vim.api.nvim_create_autocmd("ColorScheme", {
        pattern = "gruber-darker",
        callback = function()
          -- Custom colors
          vim.api.nvim_set_hl(0, "Comment", { fg = "#65737e" })
          vim.api.nvim_set_hl(0, "Whitespace", { fg = "#282828" })
          vim.api.nvim_set_hl(0, "String", { fg = "#76946A" })

          -- Make background transparent
          -- vim.api.nvim_set_hl(0, "Normal", { bg = "NONE", ctermbg = "NONE" }) --for transparent
          vim.api.nvim_set_hl(0, "Normal", { bg = "#181818" }) -- #0A0A0A
          -- vim.api.nvim_set_hl(0, "ColorColumn", { bg = "#181818", fg = "NONE" })
          -- vim.api.nvim_set_hl(0, "NormalFloat", { bg = "NONE", ctermbg = "NONE" })
          -- vim.api.nvim_set_hl(0, "NormalNC", { bg = "NONE", ctermbg = "NONE" })
          -- vim.api.nvim_set_hl(0, "SignColumn", { bg = "NONE", ctermbg = "NONE" })
          -- vim.api.nvim_set_hl(0, "LineNr", { fg = "#5f5f5f", bg = "NONE", ctermbg = "NONE" }) -- Non-active line numbers
          -- vim.api.nvim_set_hl(0, "CursorLineNr", { fg = "#ffdd33", bg = "NONE", bold = true }) -- Active line number          vim.api.nvim_set_hl(0, "Folded", { bg = "NONE", ctermbg = "NONE" })
          -- vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = "NONE", ctermbg = "NONE" })
          -- vim.api.nvim_set_hl(0, "VertSplit", { bg = "NONE", ctermbg = "NONE" })

          -- Fix indent-blankline highlight groups
          -- vim.api.nvim_set_hl(0, "IblIndent", { fg = "#3a3a3a", bg = "NONE" })
          -- vim.api.nvim_set_hl(0, "IblScope", { fg = "#5f87af", bg = "NONE" })
          -- vim.api.nvim_set_hl(0, "IblWhitespace", { fg = "#3a3a3a", bg = "NONE" })
        end,
      })
    end,
  },
  -- Kanagawa
  {
    "rebelot/kanagawa.nvim",
    name = "kanagawa",
    lazy = false,
    priority = 1000,
    opts = {
      cache = false,
      compile = true, -- enable compiling the colorscheme
      undercurl = true, -- enable undercurls(wavy underline)
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
        local palette = colors.palette
        local theme = colors.theme
        return {
          -- Neovim's color groups
          -- LineNr = { bg = "#12120F", fg = "#303446" }, -- #1D1C19 Remove bg for transparent
          String = { fg = "#76946A" },
          LineNr = { bg = "None", fg = "#3B3B35" }, -- #1D1C19 Remove bg for transparent
          CursorLineNr = { fg = "#737c73" }, --bg = "#737c73" "#12120F",  #303446 --Remove bg for transparent
          CursorLine = { bg = "#181616" }, -- 0D0C0C 252329 12120F 0e0a01 base 121F2B none 6e6a86
          -- CursorLineNr = { fg = palette.sakuraPink, bg = "NONE" },
          ColorColumn = { bg = "#0A080C", fg = "#0A080C" }, --1b161f  151118
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
        -- dark = "dragon", -- wave dragon
        -- light = "lotus", -- lotus
      },
    },
  },

  --  catppuccin
  {
    "catppuccin/nvim",
    lazy = false,
    name = "catppuccin",
    opts = {
      flavour = "latte", -- latte, frappe, macchiato, mocha
      -- flavour = "mocha", -- latte, frappe, macchiato, mocha, auto
      background = { -- :h background
        light = "latte",
        dark = "mocha",
      },
      transparent_background = false, -- if true disables setting the background color.
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

  -- BLACK METAL
  {
    "metalelf0/black-metal-theme-neovim",
    lazy = false,
    priority = 1000,
    config = function()
      -- Default options:
      require("black-metal").setup({
        -- theme = "immortal", -- blackmetal themes
        transparent = true,
        colors = { -- add/modify theme and palette colors
        },
        highlights = {
          -- ["@function"] = { fg = "#ff8800", fmt = "bold" },
          ColorColumn = { bg = "#0A080C" }, --1b161f  151118
        },
      })
    end,
  },

  -- Monotone
  {
    "Lokaltog/monotone.nvim",
    dependencies = { "rktjmp/lush.nvim" },
  },

  -- Kanagawa paper (https://github.com/thesimonho/kanagawa-paper.nvim?tab=readme-ov-file)
  {
    "thesimonho/kanagawa-paper.nvim",
    lazy = false,
    priority = 1000,
    -- init = function()
    --   vim.cmd.colorscheme("kanagawa-paper-ink")
    -- end,
    opts = {
      cache = false,
      compile = true, -- enable compiling the colorscheme
      undercurl = true, -- enable undercurls(wavy underline)
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
        local palette = colors.palette
        local theme = colors.theme
        return {
          -- Neovim's color groups
          -- LineNr = { bg = "#12120F", fg = "#303446" }, -- #1D1C19 Remove bg for transparent
          String = { fg = "#76946A" },
          LineNr = { bg = "None", fg = "#12120F" }, -- #1D1C19 Remove bg for transparent
          CursorLineNr = { fg = "#737c73" }, --bg = "#737c73" "#12120F",  #303446 --Remove bg for transparent
          CursorLine = { bg = "#181616" }, -- 0D0C0C 252329 12120F 0e0a01 base 121F2B none 6e6a86
          -- CursorLineNr = { fg = palette.sakuraPink, bg = "NONE" },
          ColorColumn = { bg = "#0A080C", fg = "#0A080C" }, --1b161f  151118
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
        -- dark = "dragon", -- wave dragon
        -- light = "lotus", -- lotus
      },
    },
  },

  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "kanagawa",
      -- colorscheme = "gruber-darker",
      -- colorscheme = "kanagawa-paper",
      -- colorscheme = "tokyonight-day",
      -- colorscheme = "catppuccin",
      -- colorscheme = "windir", -- blackmetal themes (https://github.com/metalelf0/black-metal-theme-neovim?tab=readme-ov-file#included-themes)
      -- colorscheme = "monotone",
    },
  },
}
