return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    config = function()
      require("catppuccin").setup({
        flavour = "auto", -- latte, frappe, macchiato, mocha
        background = { -- :h background
          light = "latte",
          dark = "mocha",
        },
        transparent_background = false, -- disables setting the background color.
        show_end_of_buffer = false, -- shows the '~' characters after the end of buffers
        term_colors = false, -- sets terminal colors (e.g. `g:terminal_color_0`)
        dim_inactive = {
          enabled = false, -- dims the background color of inactive window
          shade = "dark",
          percentage = 0.15, -- percentage of the shade to apply to the inactive window
        },
        no_italic = true, -- Force no italic
        no_bold = false, -- Force no bold
        no_underline = true, -- Force no underline
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
        color_overrides = {},
        custom_highlights = {},
        default_integrations = true,
        integrations = {
          cmp = true,
          gitsigns = true,
          nvimtree = true,
          treesitter = true,
          notify = false,
          mini = {
            enabled = true,
            indentscope_color = "",
          },
          -- For more plugins integrations please scroll down (https://github.com/catppuccin/nvim#integrations)
        },
      })

      -- setup must be called before loading
      vim.cmd.colorscheme("catppuccin")
    end,
  },
  {
    "rose-pine/neovim",
    name = "rose-pine",
    config = function()
      require("rose-pine").setup({
        enable = {
          terminal = true,
          legacy_highlights = true, -- Improve compatibility for previous versions of Neovim
          migrations = true, -- Handle deprecated options automatically
        },
        variant = "main", -- auto, main, moon, or dawn
        dark_variant = "moon",
        dim_inactive_windows = false,
        palette = {
          main = {
            base = "#141414", --191724  0e0a01  121018  181818
            pine = "#7287fd",
            -- pine = "#7287fd",
            -- foam = "#e4e4ef", -- e4e4ef  8caaee
            -- surface = "#1f1d2e",
            -- overlay = "#26233a",
            -- muted = "#6e6a85",
            -- subtle = "#908caa",
            -- text = "#e0def4",
            -- love = "#f43841", --"#ffdd33",
            -- gold = "#cc8c3c", -- #95a99f,
            -- iris = "#ffdd33",
          },
        },
        extend_background_behind_borders = true,
        styles = {
          bold = true,
          italic = false,
          transparency = true, -- Must not set any color
        },
        groups = {
          border = "muted",
          link = "iris",
          panel = "surface",

          error = "love",
          hint = "iris",
          info = "foam",
          note = "pine",
          todo = "rose",
          warn = "gold",

          git_add = "foam",
          git_change = "rose",
          git_delete = "love",
          git_dirty = "rose",
          git_ignore = "muted",
          git_merge = "iris",
          git_rename = "pine",
          git_stage = "iris",
          git_text = "rose",
          git_untracked = "subtle",

          h1 = "iris",
          h2 = "foam",
          h3 = "rose",
          h4 = "gold",
          h5 = "pine",
          h6 = "foam",
        },
        -- in repo: https://github.dev/rose-pine/neovim, the file: rose-pine.lua
        -- contains all the highlight group names, under the variable `default_highlights`
        highlight_groups = {
          --[[
          -- Normal = { bg = "#121018" }, -- rosepine burnt background  0e0a01
          CursorLine = { bg = "#000000" }, -- #0e0a01 base  #121F2B none   #6e6a86
          ColorColumn = { bg = "#121F2B" }, -- #0e0a01
          SignColumn = { bg = "#31748f", fg = "#f6c177" },
          LineNr = { fg = "#413630" },
          CursorLineNr = { fg = "#FFDD33" }, -- 908caa
          String = { fg = "#5A9132" }, -- 95a99f    73d936  5cbc24  43891a  70964A
          -- Set the search highlight background color
          Search = { bg = "#FFD700", fg = "#000000" }, -- Golden background with black text
          IncSearch = { bg = "#FFA500", fg = "#000000" }, -- Orange background for incremental search
          Whitespace = { fg = "#191724" },
        --]]
        },

        before_highlight = function(group, highlight, palette)
          -- Disable all undercurls
          -- if highlight.undercurl then
          --     highlight.undercurl = false
          -- end
          --
          -- Change palette colour
          -- if highlight.fg == palette.pine then
          --     highlight.fg = palette.foam
          -- end
        end,
      })
    end,
  },
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
          vim.api.nvim_set_hl(0, "Comment", { fg = "#65737e" })
          vim.api.nvim_set_hl(0, "Whitespace", { fg = "#282828" })
        end,
      })
    end,
  },
  {
    "tiagovla/tokyodark.nvim",
    name = "tokyodark",
    config = function()
      require("tokyodark").setup({
        transparent_background = true, -- set background to transparent
        gamma = 1.00, -- adjust the brightness of the theme
        styles = {
          comments = { italic = true }, -- style for comments
          keywords = { italic = false }, -- style for keywords
          identifiers = { italic = false }, -- style for identifiers
          functions = {}, -- style for functions
          variables = {}, -- style for variables
        },
        custom_highlights = {} or function(highlights, palette)
          return {}
        end, -- extend highlights
        custom_palette = {} or function(palette)
          return {}
        end, -- extend palette
        terminal_colors = true, -- enable terminal colors
      })
    end,
  },
  {
    "metalelf0/base16-black-metal-scheme",
  },
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
            LineNr = { fg = "#303446" },
            CursorLineNr = { fg = "#303446" },
            CursorLine = { bg = "#0D0C0C" }, -- 12120F 0e0a01 base 121F2B none 6e6a86
            -- SignColumn = { bg = "#ffffff", fg = "#1f1f1f" },
            -- Whitespace = { fg = "#282828" }, -- Add this line to customize the Whitespace highlight group
            WinSeparator = { fg = "#6e6a86", bg = "NONE", bold = true }, -- Customize separator color
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
    "sam4llis/nvim-tundra",
    name = "tundra",
    config = function()
      require("nvim-tundra").setup({
        transparent_background = true,
        dim_inactive_windows = {
          enabled = false,
          color = nil,
        },
        sidebars = {
          enabled = true,
          color = nil,
        },
        editor = {
          search = {},
          substitute = {},
        },
        syntax = {
          booleans = { bold = true, italic = true },
          comments = { bold = true, italic = true },
          conditionals = {},
          constants = { bold = true },
          fields = {},
          functions = {},
          keywords = {},
          loops = {},
          numbers = { bold = true },
          operators = { bold = true },
          punctuation = {},
          strings = {},
          types = { italic = true },
        },
        diagnostics = {
          errors = {},
          warnings = {},
          information = {},
          hints = {},
        },
        plugins = {
          lsp = true,
          semantic_tokens = true,
          treesitter = true,
          telescope = true,
          nvimtree = true,
          cmp = true,
          context = true,
          dbui = true,
          gitsigns = true,
          neogit = true,
          textfsm = true,
        },
        overwrite = {
          colors = {},
          highlights = {},
        },
      })
      vim.g.tundra_biome = "arctic" -- 'arctic' or 'jungle'
    end,
  },
  {
    "LazyVim/LazyVim",
    opts = {
      -- colorscheme = "tundra",
      colorscheme = "kanagawa",
      -- colorscheme = "rose-pine",
      -- colorscheme = "catppuccin-latte",
      -- colorscheme = "gruber-darker",
      -- colorscheme = "tokyodark",
      -- colorscheme = "GruberDarker",
      -- colorscheme = "brightburn",
      -- colorscheme = "shiva_metal",
      -- colorscheme = "base16-black-metal-mayhem", -- venom nile  mayhem  marduk  immortal  dark-funeral
    },
  },
}
