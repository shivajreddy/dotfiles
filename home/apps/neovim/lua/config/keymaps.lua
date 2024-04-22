-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set("n", "<C-n>", "<cmd>Neotree toggle<CR>", { desc = "Neotree Toggle" })

vim.api.nvim_set_keymap("n", "<leader>as", ":ASToggle<CR>", {})

--[[ :: Using Smart-splits ::
vim.keymap.set("n", "<C-h>", require("smart-splits").move_cursor_left)
vim.keymap.set("n", "<C-j>", require("smart-splits").move_cursor_down)
vim.keymap.set("n", "<C-k>", require("smart-splits").move_cursor_up)
vim.keymap.set("n", "<C-l>", require("smart-splits").move_cursor_right)
vim.keymap.set("n", "<C-\\>", require("smart-splits").move_cursor_previous)
-- ]]

-- Using TmuxNavigator
vim.keymap.set("n", "<C-h>", "<cmd>TmuxNavigateLeft<cr>", {})
vim.keymap.set("n", "<C-j>", "<cmd><C-U>TmuxNavigateDown<cr>", {})
vim.keymap.set("n", "<C-k>", "<cmd><C-U>TmuxNavigateUp<cr>", {})
vim.keymap.set("n", "<C-l>", "<cmd><C-U>TmuxNavigateRight<cr>", {})
