-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- vim.keymap.set("n", "", "", { desc = "" })

--  Lazy
vim.keymap.set("n", "<Leader>L", ":Lazy<CR>", { desc = "Lazy", silent = true })

-- Live Grep
vim.keymap.set("n", "<Leader>l", "", { desc = "Live Grep", silent = true })

-- Comment
vim.keymap.set("n", "<Leader>/", ":gc<CR>", { desc = "Comment Line", silent = true })
