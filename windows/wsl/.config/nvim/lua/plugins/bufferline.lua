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
      show_buffer_close_icons = true,
      -- separator_style = "slant",
    },

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
