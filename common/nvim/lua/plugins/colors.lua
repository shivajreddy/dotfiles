return {
  {
    "rose-pine/neovim",
    name = "rose-pine",
    config = function()
      require("rose-pine").setup({
        variant = "main", -- auto, main, moon, or dawn

        palette = {
          main = {
            base = "#181818", --191724  0e0a01  121018
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

        styles = {
          -- bold = false,
          italic = true,
          -- transparency = true, -- Must not set any color
          extend_background_behind_borders = true,
        },
        -- in repo: https://github.dev/rose-pine/neovim, the file: rose-pine.lua
        -- contains all the highlight group names, under the variable `default_highlights`
        highlight_groups = {
          -- Normal = { bg = "#121018" }, -- rosepine burnt background  0e0a01
          --[[
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
  },
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "rose-pine",
      -- colorscheme = "gruber-darker",
      -- colorscheme = "GruberDarker",
      -- colorscheme = "tokyodark",
      -- colorscheme = "brightburn",
      -- colorscheme = "shiva_metal",
      -- colorscheme = "base16-black-metal-marduk", -- venom nile  mayhem  marduk  immortal
    },
  },
}
