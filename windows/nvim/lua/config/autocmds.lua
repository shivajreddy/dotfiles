-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

vim.api.nvim_create_autocmd("BufWinEnter", {
  command = "set formatoptions-=cro",
})

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- Create an autogroup named "autoupdate"
local autoupdate_group = augroup("autoupdate", { clear = true })

-- Define an autocommand that runs when Vim enters
autocmd("VimEnter", {
  group = autoupdate_group,
  callback = function()
    require("lazy").update({ show = false })
  end,
})

-- Set a custom background color of Man pages buffer
vim.api.nvim_create_autocmd("FileType", {
  pattern = "man",
  callback = function()
    vim.wo.winhighlight = "Normal:ManNormal,CursorLine:ManCursorLine"
    vim.cmd("hi ManNormal guibg=#181818") -- background color
    vim.cmd("hi ManCursorLine guibg=#282828") -- cursor line color
  end,
})

-- Hide line numbers for markdown and text files using BufEnter to match by file extension
vim.api.nvim_create_autocmd("BufEnter", {
  pattern = { "*.md", "*.txt" },
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
  end,
})

-- Deletes the autocommand that comes with lazyvim, with the group name "lazyvim_wrap_spell"
-- this autocommand enables spellcheck for text, markdown files
vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")
