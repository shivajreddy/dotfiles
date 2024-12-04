-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- This will disable new line to also have comment
-- https://www.reddit.com/r/neovim/comments/191l9bb/how_do_i_integrate_set_formatoptionscro_in_lazyvim/
vim.opt.formatoptions:remove({ "c", "r", "o" })

vim.opt.clipboard = "unnamedplus"
-- vim.opt.clipboard:append("unnamedplus")

vim.opt.list = false

vim.opt.sidescroll = 1
vim.opt.sidescrolloff = 10

-- Disable right-click mouse functionality (doublicking on right will make a single click)
vim.api.nvim_set_keymap("n", "<RightMouse>", "<Nop>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("i", "<RightMouse>", "<Nop>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("v", "<RightMouse>", "<Nop>", { noremap = true, silent = true })

vim.opt.nu = true
vim.opt.relativenumber = true

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.hlsearch = true
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")

vim.opt.updatetime = 50

-- vim.opt.colorcolumn = "80"
vim.opt.colorcolumn = "100"

vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
