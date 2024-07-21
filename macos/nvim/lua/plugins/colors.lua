-- rose useful link
-- https://www.reddit.com/r/neovim/comments/19f7s7e/changing_default_ros%C3%A9_pine_colours/
return {
  {
    "rose-pine/neovim",
    name = "rose-pine",
    config = function()
      require("rose-pine").setup({
        variant = "auto", -- auto, main, moon, or dawn
        styles = {
          bold = true,
          italic = true,
          transparency = true,
        },
        groups = {
          h1 = "love",
        },
        highlight_groups = {
          -- Normal = { bg = "#061111" },
          -- ["String"] = { fg = "#27d653" },
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
      -- flavour = "latte", -- latte, frappe, macchiato, mocha
      flavour = "mocha", -- latte, frappe, macchiato, mocha
      background = { -- :h background
        light = "latte",
        dark = "mocha",
      },
      transparent_background = true, -- disables setting the background color.
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
      colorscheme = "rose-pine",
      -- colorscheme = "catppuccin",
    },
  },
}
