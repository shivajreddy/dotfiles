return {
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
            -- base = "#0E0A01", --191724  0E0A01  121018  181818  141414
            -- pine = "#73c936", --7287fd  89B4FA
            -- foam = "#e4e4ef", -- 8caaee
            -- rose = "#cc8c3c",
            -- surface = "#1f1d2e",
            -- overlay = "#26233a",
            -- muted = "#6e6a85",
            -- subtle = "#908caa",
            -- text = "#e0def4",
            -- love = "#f43841", --ffdd33
            -- gold = "#FFDD33", -- 95a99f  cc8c3c
            -- iris = "#ffdd33",
          },
        },
        extend_background_behind_borders = true,
        styles = {
          bold = true,
          italic = true,
          transparency = true, -- Must not set any color, true means use terminal color
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
        highlight_groups = {},

        before_highlight = function(group, highlight, palette)
          -- Disable all undercurls
          if highlight.undercurl then
            highlight.undercurl = false
          end
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
    "LazyVim/LazyVim",
    opts = {
      -- colorscheme = "tokyodark",
      -- colorscheme = "rose-pine",
      colorscheme = "gruber-darker",
      -- colorscheme = "GruberDarker",
      -- colorscheme = "brightburn",
      -- colorscheme = "shiva_metal",
      -- colorscheme = "base16-black-metal-marduk", -- venom nile  mayhem  marduk  immortal
    },
  },
}
