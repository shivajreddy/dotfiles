-- These all my general remaps, plugin specific remaps are at plugin level

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Set to true if you have a Nerd Font installed
vim.g.have_nerd_font = true

-- Make line numbers default
vim.opt.number = true
vim.opt.relativenumber = true
-- You can also add relative line numbers, for help with jumping.
--  Experiment for yourself to see if you like it!


vim.api.nvim_set_keymap('i', '<CR>', '<C-y>', { noremap = true, silent = true })

-- Enable mouse mode, can be useful for resizing splits for example!
vim.opt.mouse = 'a'

-- Don't show the mode, since it's already in status line
vim.opt.showmode = false

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.opt.clipboard = 'unnamedplus'

-- Enable break indent
vim.opt.breakindent = true

-- Save undo history
vim.opt.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Keep signcolumn on by default
vim.opt.signcolumn = 'yes'

-- Decrease update time
vim.opt.updatetime = 250
vim.opt.timeoutlen = 300

-- Configure how new splits should be opened
vim.opt.splitright = true
vim.opt.splitbelow = true

-- Sets how neovim will display certain whitespace in the editor.
--  See `:help 'list'`
--  and `:help 'listchars'`
--  NOTE: Not why this is not working
--  Not 
_G.toggle_white_space = function()
  if vim.opt.list[1] then
    -- vim.opt.list = false
    vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }
  else
    vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }
    -- vim.opt.list = true
  end
end
vim.api.nvim_set_keymap('n', '<leader>l', ':lua toggle_white_space()<CR>', {noremap = true, silent = true})

-- Preview substitutions live, as you type!
vim.opt.inccommand = 'split'

-- Show which line your cursor is on
vim.opt.cursorline = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 5

-- [[ Basic Keymaps ]]
--  See `:help vim.keymap.set()`

-- Set highlight on search, but clear on pressing <Esc> in normal mode
vim.opt.hlsearch = true


