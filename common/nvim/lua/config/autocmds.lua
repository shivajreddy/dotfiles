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

--[[
-- only works when opt.signcolumn="yes"
vim.api.nvim_create_autocmd({ "BufEnter", "WinEnter" }, {
  callback = function()
    local separator = " ▎"
    vim.opt.statuscolumn = '%s%=%#LineNr4#%{(v:relnum >= 4)?v:relnum."'
      .. separator
      .. '":""}'
      .. '%#LineNr3#%{(v:relnum == 3)?v:relnum."'
      .. separator
      .. '":""}'
      .. '%#LineNr2#%{(v:relnum == 2)?v:relnum."'
      .. separator
      .. '":""}'
      .. '%#LineNr1#%{(v:relnum == 1)?v:relnum."'
      .. separator
      .. '":""}'
      .. '%#LineNr0#%{(v:relnum == 0)?v:lnum." '
      .. separator
      .. '":""}'
    -- vim.cmd("highlight LineNr0 guifg=#dedede")
    -- vim.cmd("highlight LineNr1 guifg=#bdbdbd")
    -- vim.cmd("highlight LineNr2 guifg=#9c9c9c")
    -- vim.cmd("highlight LineNr3 guifg=#7b7b7b")
    -- vim.cmd("highlight LineNr4 guifg=#5a5a5a")
  end,
})
--]]
