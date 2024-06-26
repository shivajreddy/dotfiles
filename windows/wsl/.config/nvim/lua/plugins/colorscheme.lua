-- rose useful link
-- https://www.reddit.com/r/neovim/comments/19f7s7e/changing_default_ros%C3%A9_pine_colours/
return {
  {
    "rose-pine/neovim",
    name = "rose-pine",
    config = function()
      require('rose-pine').setup({
        styles = {
          bold = true,
          italic = true,
          transparency = true
        },
        groups = {
          h1 = "love"
        },
        highlight_groups = {
          -- Normal = { bg = "#061111" },
          -- ["String"] = { fg = "#27d653" },
        }
      })
    end
  },

  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "rose-pine"
    }
  }
}
