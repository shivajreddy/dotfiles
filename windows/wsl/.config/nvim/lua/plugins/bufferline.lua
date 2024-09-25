-- local mocha = require("catppuccin.palettes").get_palette("mocha")

local bufferline = require("bufferline")
local mocha = require("catppuccin.palettes").get_palette("mocha")

return {
  "akinsho/bufferline.nvim",
  event = "VeryLazy",
  opts = {
    options = {
      diagnostics = "nvim_lsp",
      -- diagnostics_indicator = function(count, level, diagnostics_dict, context)
      --   local icon = level:match("error") and "󰅙 " or " "
      --   return " " .. icon .. count
      -- end,
      themable = true,
      separator_style = "thick", -- "thick"  "slant"
      -- indicator = { style = "underline" },
      always_show_bufferline = false,
      style_preset = bufferline.style_preset.default,
      -- style_preset = bufferline.style_preset.no_italic,
      show_buffer_close_icons = false,
    },

    -- Customize Catppuccin colors
    -- https://github.com/catppuccin/nvim?tab=readme-ov-file
    highlights = require("catppuccin.groups.integrations.bufferline").get({
      styles = { "italic", "bold" },
      custom = {
        all = {
          fill = { bg = "#000000" },
        },
        mocha = {
          background = { fg = mocha.text },
        },
        latte = {
          background = { fg = "#000000" },
        },
      },
    }),
  },
}
