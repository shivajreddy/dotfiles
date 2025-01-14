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
        dim_inactive_windows = true,
        palette = {
          main = {
            base = "#141414", --191724  0e0a01  121018  181818
            pine = "#7287fd",
            -- pine = "#7287fd",
            -- foam = "#8caaee",
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
          italic = true,
          -- transparency = true, -- Must not set any color
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
    "LazyVim/LazyVim",
    opts = {
      -- colorscheme = "tokyodark",
      colorscheme = "rose-pine",
      -- colorscheme = "gruber-darker",
      -- colorscheme = "GruberDarker",
      -- colorscheme = "brightburn",
      -- colorscheme = "shiva_metal",
      -- colorscheme = "base16-black-metal-marduk", -- venom nile  mayhem  marduk  immortal
    },
  },
}
