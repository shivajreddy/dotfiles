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
