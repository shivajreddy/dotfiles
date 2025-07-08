-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
-- This will disable new line to also have comment
-- https://www.reddit.com/r/neovim/comments/191l9bb/how_do_i_integrate_set_formatoptionscro_in_lazyvim/
vim.opt.formatoptions:remove({ "c", "r", "o" })

-- vim.opt.clipboard:append("unnamedplus")
-- vim.opt.clipboard = "unnamedplus" --Works for linux
-- vim.g.clipboard = { --Works for wsl
--   name = "win32yank",
--   copy = {
--     ["+"] = "win32yank.exe -i --crlf",
--     ["*"] = "win32yank.exe -i --crlf",
--   },
--   paste = {
--     ["+"] = "win32yank.exe -o --lf",
--     ["*"] = "win32yank.exe -o --lf",
--   },
--   cache_enabled = 0,
-- }

local is_wsl = vim.fn.has("wsl") == 1
local sysname = vim.loop.os_uname().sysname
if is_wsl or sysname == "Windows_NT" then
  vim.g.clipboard = {
    name = "win32yank",
    copy = {
      ["+"] = "win32yank.exe -i --crlf",
      ["*"] = "win32yank.exe -i --crlf",
    },
    paste = {
      ["+"] = "win32yank.exe -o --lf",
      ["*"] = "win32yank.exe -o --lf",
    },
    cache_enabled = 0,
  }
elseif sysname == "Linux" then
  vim.opt.clipboard = "unnamedplus"
elseif sysname == "Darwin" then
  vim.opt.clipboard = "unnamedplus" -- or customize for macOS if needed
else
  vim.notify("No clipboard integration configured for OS: " .. sysname, vim.log.levels.WARN)
end

-- Disable options

-- Animations
vim.g.snacks_animate = true

vim.opt.sidescroll = 1
vim.opt.sidescrolloff = 10

-- Disable right-click mouse functionality (doublicking on right will make a single click)
vim.api.nvim_set_keymap("n", "<RightMouse>", "<Nop>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("i", "<RightMouse>", "<Nop>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("v", "<RightMouse>", "<Nop>", { noremap = true, silent = true })

vim.opt.number = true -- line numbers
vim.opt.relativenumber = true --  relative line numbers

-- General indentation settings
-- vim.opt.tabstop = 4
-- vim.opt.softtabstop = 4
-- vim.opt.shiftwidth = 4
vim.opt.expandtab = true -- Use spaces instead of tabs
vim.opt.tabstop = 4 -- Width of a tab character
vim.opt.shiftwidth = 4 -- Indentation level
vim.opt.softtabstop = 4 -- Tab key behaves like inserting 2 spaces
vim.opt.autoindent = true -- Maintain indentation on new lines

vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.hlsearch = true
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")

vim.opt.updatetime = 50

vim.opt.colorcolumn = "80" -- traditional
-- vim.opt.colorcolumn = "100" -- google style guide

vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

-- White Space
vim.opt.list = false
vim.opt.listchars = {
  space = "⋅", -- Show spaces as dots
  tab = "» ", -- Show tabs with a special character and space
  trail = "⋅", -- Show trailing spaces
  extends = "→", -- Show character for text that extends beyond the window
  precedes = "←", -- Show character for text that precedes the window
  nbsp = "␣", -- Show non-breaking space
}

-- Show signcolumn(signcolumn is used to show the gutter symbols)
vim.opt.signcolumn = "no" -- "yes" "no"
-- Add a gutter line next to line number
-- vim.opt.statuscolumn = "%=" .. "%{printf('%3s', v:relnum ? v:relnum : v:lnum)} ▕"
