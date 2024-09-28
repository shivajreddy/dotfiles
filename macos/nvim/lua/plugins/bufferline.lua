-- local mocha = require("catppuccin.palettes").get_palette("mocha")

local bufferline = require("bufferline")

return {
  "akinsho/bufferline.nvim",
  event = "VeryLazy",
  opts = {
    options = {
      -- diagnostics = "nvim_lsp",
      -- diagnostics_indicator = function(count, level, diagnostics_dict, context)
      --   local icon = level:match("error") and "󰅙 " or " "
      --   return " " .. icon .. count
      -- end,
      themable = true,
      separator_style = "thick",
      -- indicator = {
      --   style = "underline",
      -- },
      always_show_bufferline = false,
      -- style_preset = bufferline.style_preset.default,
      style_preset = bufferline.style_preset.no_italic,
      show_buffer_close_icons = false,
      -- separator_style = "slant",
    },

    -- Light Theme
    --[[
    highlights = {
      tab_separator = {
        bg = "#9ca0b0",
      },
      tab_selected = {
        bg = "#9ca0b0",
      },
      tab_separator_selected = {
        bg = "#9ca0b0",
      },
      close_button_selected = {
        bg = "#9ca0b0",
      },
      buffer_selected = {
        bg = "#9ca0b0",
      },
      numbers_selected = {
        bg = "#9ca0b0",
      },
      diagnostic_selected = {
        bg = "#9ca0b0",
      },
      hint_selected = {
        bg = "#9ca0b0",
      },
      hint_diagnostic_selected = {
        bg = "#9ca0b0",
      },
      info_selected = {
        bg = "#9ca0b0",
      },
      info_diagnostic_selected = {
        bg = "#9ca0b0",
      },
      warning_selected = {
        bg = "#9ca0b0",
      },
      warning_diagnostic_selected = {
        bg = "#9ca0b0",
      },
      error_selected = {
        bg = "#9ca0b0",
      },
      error_diagnostic_selected = {
        bg = "#9ca0b0",
      },
      modified_selected = {
        bg = "#9ca0b0",
      },
      duplicate_selected = {
        bg = "#9ca0b0",
      },
      separator_selected = {
        bg = "#9ca0b0",
      },
      indicator_selected = {
        bg = "#9ca0b0",
      },
      pick_selected = {
        bg = "#9ca0b0",
      },
    },
    --]]

    -- dark theme
    -- --[[
    highlights = {
      tab_separator = {
        bg = "#2a273f",
      },
      tab_selected = {
        bg = "#2a273f",
      },
      tab_separator_selected = {
        bg = "#2a273f",
      },
      close_button_selected = {
        bg = "#2a273f",
      },
      buffer_selected = {
        bg = "#2a273f",
      },
      numbers_selected = {
        bg = "#2a273f",
      },
      diagnostic_selected = {
        bg = "#2a273f",
      },
      hint_selected = {
        bg = "#2a273f",
      },
      hint_diagnostic_selected = {
        bg = "#2a273f",
      },
      info_selected = {
        bg = "#2a273f",
      },
      info_diagnostic_selected = {
        bg = "#2a273f",
      },
      warning_selected = {
        bg = "#2a273f",
      },
      warning_diagnostic_selected = {
        bg = "#2a273f",
      },
      error_selected = {
        bg = "#2a273f",
      },
      error_diagnostic_selected = {
        bg = "#2a273f",
      },
      modified_selected = {
        bg = "#2a273f",
      },
      duplicate_selected = {
        bg = "#2a273f",
      },
      separator_selected = {
        bg = "#2a273f",
      },
      indicator_selected = {
        bg = "#2a273f",
      },
      pick_selected = {
        bg = "#2a273f",
      },
    },
    -- --]]

    --[[
		highlights = require("catppuccin.groups.integrations.bufferline").get({
			styles = { "italic", "bold" },
			custom = {
				all = {
					fill = { bg = "#000000" },
				},
				mocha = {
					-- buffer_visible = { bg = "#b4befe", fg = "#11111b" },
					buffer_selected = { bg = "#6c7086", fg = "#11111b" },
					-- indicator_selected = { fg = "#89b4fa" },
					-- separators
					separator = { bg = "#6c7086", fg = "#11111b" },
					separator_visible = { bg = "#6c7086", fg = "#11111b" },
					separator_selected = { bg = "#6c7086", fg = "#11111b" },
					offset_separator = { bg = "#6c7086", fg = "#11111b" },
				},
				latte = {
					background = { fg = "#000000" },
				},
			},
		}),
    --]]

    -- Customize Catppuccin colors
    -- https://github.com/catppuccin/nvim?tab=readme-ov-file
    -- after = "catppuccin",
    -- config = function()
    -- 	require("bufferline").setup({
    -- 		highlights = require("catppuccin.groups.integrations.bufferline").get(),
    -- 	})
    -- end,
  },
}
