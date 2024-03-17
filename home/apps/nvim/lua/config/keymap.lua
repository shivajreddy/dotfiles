vim.g.mapleader = "<Space>"

vim.keymap.set("n", "<Leader>/", "gc<CR>", { desc = "Comment Line", silent = true })

vim.keymap.set("n", "Y", "y$", { desc = "Yank end of line", silent = true })
vim.keymap.set("n", "<leader>pv", vim.cmd.Explore, { desc = "Open NetRW", silent = true })

-- Not working for somereason
vim.keymap.set("n", "<Left>", "<C-w>h<CR>", { desc = "Tab Left" })
vim.keymap.set("n", "<Down>", "<C-w>j<CR>", { desc = "Tab Down" })
vim.keymap.set("n", "<Up>", "<C-w>k<CR>", { desc = "Tab Up" })
vim.keymap.set("n", "<Right>", "<C-w>l<CR>", { desc = "Tab Right" })

-- Not working for somereason
vim.api.nvim_set_keymap("n", "<Leader>w", ":w<CR>", { desc = "Write", silent = true })
vim.api.nvim_set_keymap("n", "<Leader>wq", ":wq<CR>", { desc = "Write & Quit", silent = true })
