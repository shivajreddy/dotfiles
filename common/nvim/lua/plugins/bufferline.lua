return {
  "akinsho/bufferline.nvim",
  dependencies = { "catppuccin/nvim" },
  opts = function(_, opts)
    -- local active_bg = "#DA3B01"
    -- local active_fg = "#000000"
    -- local active_fg = "#DA3B01"
    local active_bg = "#181825"
    -- local active_fg = "#ffffff"

    opts.highlights = require("catppuccin.special.bufferline").get_theme({
      styles = { "bold" }, -- bold only, no italic
      custom = {
        all = {
          buffer_selected = { fg = active_fg, bg = active_bg },
          tab_selected = { fg = active_fg, bg = active_bg },
          numbers_selected = { fg = active_fg, bg = active_bg },
          close_button_selected = { fg = active_fg, bg = active_bg },
          modified_selected = { fg = active_fg, bg = active_bg },
          indicator_selected = { fg = active_bg, bg = active_bg },
          duplicate_selected = { fg = active_fg, bg = active_bg },
          separator_selected = { fg = active_bg, bg = active_bg },
          tab_separator_selected = { fg = active_bg, bg = active_bg },
          diagnostic_selected = { fg = active_fg, bg = active_bg },
          hint_selected = { fg = active_fg, bg = active_bg },
          hint_diagnostic_selected = { fg = active_fg, bg = active_bg },
          info_selected = { fg = active_fg, bg = active_bg },
          info_diagnostic_selected = { fg = active_fg, bg = active_bg },
          warning_selected = { fg = active_fg, bg = active_bg },
          warning_diagnostic_selected = { fg = active_fg, bg = active_bg },
          error_selected = { fg = active_fg, bg = active_bg },
          error_diagnostic_selected = { fg = active_fg, bg = active_bg },
        },
      },
    })

    return opts
  end,
}
