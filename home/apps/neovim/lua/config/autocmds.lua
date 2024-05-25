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

vim.opt.colorcolumn = 80
