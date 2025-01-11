local bufferline = require("bufferline")

return {
  "akinsho/bufferline.nvim",
  enabled = false,
  opts = {
    options = {
      show_buffer_close_icons = false,
      style_preset = {
        bufferline.style_preset.no_italic,
        bufferline.style_preset.minimal,
      },
    },
  },
}
