
/*
-- options.lua
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.o.clipboard = 'unnamedplus'

vim.o.number = true
-- vim.o.relativenumber = true

vim.o.signcolumn = 'yes'

vim.o.tabstop = 4
vim.o.shiftwidth = 4

vim.o.updatetime = 300

vim.o.termguicolors = true

vim.o.mouse = 'a'


vim.keymap.set('n', '<Leader>q', '<Cmd>q<CR>')
vim.keymap.set('n', '<Leader>s', '<Cmd>w<CR>')
vim.keymap.set('n', '<Leader>wq', '<Cmd>wq<CR>')
*/

[
	{
		action = "<cmd>q<CR>";
		key = "<Leader>q";
		options.desc = "Quit";
	}
	{
		# change to auto-save plugin toggle
		action = "<cmd>w<CR>";
		key = "<Leader>s";
		options.desc = "Write";
	}
	{
		action = "<cmd>wq<CR>";
		key = "<Leader>wq";
		options.desc = "Write&Quit";
	}
	{
		action = "<cmd>Telescope live_grep<CR>";
		key = "<Leader>sg";
		options.desc = "Telescope::LiveGrep";
	}
	{
		action = "<cmd>Neotree toggle<CR>";
		key = "<C-n>";
		options.desc = "Neotree Toggle";
	}
]
