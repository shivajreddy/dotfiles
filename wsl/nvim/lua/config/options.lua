-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- This will disable new line to also have comment
-- https://www.reddit.com/r/neovim/comments/191l9bb/how_do_i_integrate_set_formatoptionscro_in_lazyvim/
vim.opt.formatoptions:remove({ "c", "r", "o" })

vim.o.scrolloff = 8

vim.opt.updatetime = 50

vim.opt.colorcolumn = "80"
-- vim.opt.colorcolumn = "100"

vim.opt.relativenumber = false

vim.opt.clipboard:append("unnamedplus")

vim.opt.list = false
