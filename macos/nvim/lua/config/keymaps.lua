-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- General
-- vim.api.nvim_set_keymap("n", "I", vim.diagnostic.open_float, { desc = "Line Diagnostics" })
vim.keymap.set("n", "I", vim.diagnostic.open_float, { desc = "Line Diagnostics" })

-- custom keymaps to run c file
vim.api.nvim_create_user_command("MakeRun", "!term make run", {})
vim.api.nvim_set_keymap(
  "n",
  "<leader>rr",
  ":term make run<CR>",
  { noremap = true, silent = true, desc = "Exec `make run`" }
)

-- Define the toggle_hidden_all function
_G.toggle_hidden_all = function()
  if not _G.hidden_all then
    _G.hidden_all = true
    -- vim.o.showmode = false
    -- vim.o.ruler = false
    vim.o.laststatus = 0
    -- vim.o.showcmd = false
  else
    _G.hidden_all = false
    -- vim.o.showmode = true
    -- vim.o.ruler = true
    vim.o.laststatus = 2
    -- vim.o.showcmd = true
  end
end

-- Initialize the hidden_all variable
_G.hidden_all = false

vim.api.nvim_set_keymap(
  "n",
  "<leader>uS",
  ":lua toggle_hidden_all()<CR>",
  { noremap = true, silent = true, desc = "Toggle StatusLine" }
)

--[[
-- https://www.reddit.com/r/neovim/comments/15a7wyt/is_it_possible_to_scroll_horizontally_with_the/
noremap <C-ScrollWheelDown> 10zl
noremap <C-2-ScrollWheelDown> 10zl
noremap <C-3-ScrollWheelDown> 10zl
noremap <C-4-ScrollWheelDown> 10zl
noremap <C-ScrollWheelUp> 10zh
noremap <C-2-ScrollWheelUp> 10zh
noremap <C-3-ScrollWheelUp> 10zh
noremap <C-4-ScrollWheelUp> 10zh
--]]
-- vim.keymap.set("n", "<S-ScrollWheelUp>", zH, {})
-- vim.keymap.set("n", "<S-ScrollWheelDown>", zL, {})
vim.keymap.set("n", "<S-ScrollWheelUp>", "10zh", {})
vim.keymap.set("n", "<S-ScrollWheelDown>", "10zL", {})
vim.keymap.set("n", "<S-2-ScrollWheelDown>", "10zL", {})
vim.keymap.set("n", "<S-3-ScrollWheelDown>", "10zL", {})
vim.keymap.set("n", "<S-4-ScrollWheelDown>", "10zL", {})

vim.keymap.set("n", "<C-n>", "<cmd>Neotree toggle<CR>", { desc = "Neotree Toggle" })

-- For Mac users, map Command+S to save
-- zellij doesn't detect command key, so we remapped at alacritty level
-- so when cmd+s is pressed alacritty treats as ctrl+s
vim.api.nvim_set_keymap("n", "<C-s>", ":w<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("i", "<C-s>", "<C-o>:w<CR>", { noremap = true, silent = true })

-- Autosave toggle
vim.keymap.set("n", "<leader>as", ":ASToggle<CR>", {})
-- GitSigns toggle
vim.keymap.set("n", "<leader>ug", ":Gitsigns toggle_signs<CR>", { desc = "Toggle GitSigns" })

-- ZenMode
vim.api.nvim_set_keymap("n", "<leader>zz", "<CMD>ZenMode<CR>", { desc = "ZenMode", silent = false })

-- Split windows
vim.keymap.set("n", "<Leader>w'", "<cmd>split<CR>")
vim.keymap.set("n", '<Leader>w"', "<cmd>vs<CR>")

-- Close
vim.keymap.set("n", "<Leader>wq", "<cmd>bdelete<CR>")
vim.keymap.set("n", "<Leader>x", "<cmd>bdelete<CR>")
-- vim.keymap.set("n", "<Leader>wq", "<cmd>wq<CR>")
-- vim.keymap.set("n", "<Leader>wx", "<cmd>q<CR>")

-- LazyTerm
vim.keymap.set("n", "<C-/>", function()
  Util.terminal(nil, { border = "rounded" })
end, { desc = "LazyTerm with Border", silent = false })

-- All Text Controls
function YankEntireBuffer()
  local cursor_pos = vim.api.nvim_win_get_cursor(0) -- Save cursor position
  vim.cmd(":%yank") -- Yank the entire buffer
  vim.api.nvim_win_set_cursor(0, cursor_pos) -- Restore cursor position
end
vim.keymap.set("n", "<Leader>ay", [[:lua YankEntireBuffer()<CR>]], { desc = "Yank all" })
vim.keymap.set("n", "<Leader>ad", ":%d<CR>")
vim.keymap.set("n", "<Leader>aa", "gg<S-v>G")

--[[ :: Using Smart-splits ::
vim.keymap.set("n", "<C-h>", require("smart-splits").move_cursor_left)
vim.keymap.set("n", "<C-j>", require("smart-splits").move_cursor_down)
vim.keymap.set("n", "<C-k>", require("smart-splits").move_cursor_up)
vim.keymap.set("n", "<C-l>", require("smart-splits").move_cursor_right)
vim.keymap.set("n", "<C-\\>", require("smart-splits").move_cursor_previous)
-- ]]

-- Prime's Keymaps: https://github.com/Eandrju/cellular-automaton.nvim
-- Open Neovim Explorer
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

-- dont change cursor position when using J
vim.keymap.set("n", "J", "mzJ`z")

-- half page jumping with centering screen
-- vim.api.nvim_set_keymap("n", "<C-u>", "<C-u>zz", { noremap = true, silent = true })
-- vim.api.nvim_set_keymap("n", "<C-d>", "<C-d>zz", { noremap = true, silent = true })
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- searching with n, N will also center the screen
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- preserve copy buffer, when you paster over highligted
vim.keymap.set("x", "<leader>p", [["_dP]])
-- delete to void register so that when you leader+d you dont add that delete to yank
vim.keymap.set({ "n", "v" }, "<leader>d", [["_d]])

-- leader+y or leader+Y to yank to clipboard, and y,Y to yank with in vim
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

-- <nop> means no operation, so we are disabling Q. cuz by default Q is recording
vim.keymap.set("n", "Q", "<nop>")

-- switch projects via tmux
-- vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")
-- vim.keymap.set("n", "<leader>f", vim.lsp.buf.format)

-- quick fix navigation, not sure wtf this is
-- vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
-- vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")

-- change every occurance of the word under cursor
-- vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
