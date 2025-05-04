-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

-- https://github.com/LazyVim/LazyVim/issues/2491

--[[
vim.api.nvim_create_autocmd({ "InsertLeave", "TextChanged" }, {
	pattern = { "*" },
	command = "silent! wall",
	nested = true,
})
--]]

-- vim.api.nvim_create_autocmd("BufWinEnter", {
-- 	command = "set formatoptions-=cro",
-- })

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

-- Disable white space in selection mode
--[[
vim.api.nvim_create_autocmd("ModeChanged", {
  pattern = "*",
  callback = function()
    if vim.fn.mode() == "v" or vim.fn.mode() == "V" or vim.fn.mode() == "" then
      vim.opt.list = false -- Disable whitespace highlighting
    else
      vim.opt.list = true -- Re-enable whitespace highlighting
    end
  end,
})
--]]

-- Run BufferOrderByBufferNumber command every time a buffer is open or closed
-- this ain't working for somereason
-- vim.api.nvim_create_autocmd({ "BufAdd", "BufDelete" }, {
--   callback = function()
--     vim.cmd("BufferOrderByBufferNumber")
--   end,
-- })
