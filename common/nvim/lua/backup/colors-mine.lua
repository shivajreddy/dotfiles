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
          -- h1 = "love",
        },
        -- in repo: https://github.dev/rose-pine/neovim, the file: rose-pine.lua
        -- contains all the highlight group names, under the variable `default_highlights`
        highlight_groups = {
          Normal = { bg = "#0e0a01" }, -- rosepine burnt background
          -- Normal = { bg = "#14110F" }, -- smoky black
          CursorLine = { bg = "base" },
          ColorColumn = { bg = "#0e0a01" },
          -- VertSplit = { fg = "muted", bg = "muted" },
          -- Normal = { bg = "#061111" },
          -- ["String"] = { fg = "#27d653" },
          -- ["SignColumn"] = { bg = "#31748f", fg = "#f6c177" },
          -- ["SignColumn"] = { bg = "#31748f" },
          SignColumn = { bg = "#31748f", fg = "#f6c177" },
          -- CursorLine = { bg = "#31748f", fg = "#f6c177" },
          -- CursorLine = { bg = "#0e0a01" },
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
      -- flavour = "latte", -- latte, frappe, macchiato, mocha
      flavour = "mocha", -- latte, frappe, macchiato, mocha
      background = { -- :h background
        light = "latte",
        dark = "mocha",
      },
      transparent_background = false, -- disables setting the background color.
      show_end_of_buffer = false, -- shows the '~' characters after the end of buffers
      term_colors = true, -- sets terminal colors (e.g. `g:terminal_color_0`)
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
        all = {
          -- base = "#130d02",
          base = "#14110F",
          -- base = "#11111b",
          -- mantle = "#191724",
          -- crust = "#191724",
        },
      },
      highlight_overrides = {
        all = function(colors)
          return {
            -- NeoTree
            -- NeoTreeNormal = { bg = "#11111b" },
            -- NeoTreeNormalNC = { bg = "#11111b" },
            CursorLine = { bg = "NONE" }, -- No background color for the entire line
            CursorColumn = { bg = colors.mantle }, -- Apply color to the column of the cursor
          }
        end,
      },
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
      colorscheme = "rose-pine",
      -- colorscheme = "rose-pine-moon",
      -- colorscheme = "catppuccin",
      -- colorscheme = "catppuccin-mocha",
      -- colorscheme = "brightburn",
    },
  },
}
