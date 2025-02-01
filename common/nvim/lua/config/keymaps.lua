-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

--#region keymap to run a custom command
vim.keymap.set("n", "<F5>", function()
  -- Get the current file's directory
  local current_file = vim.fn.expand("%:p:h")

  -- Change to the directory of the current file
  vim.cmd("cd " .. current_file)

  -- Run the build.bat file
  local command = "build.bat"
  vim.fn.system(command)

  -- Optionally, you can print a message to confirm the build was run
  print("Build command executed: " .. command)
end, { noremap = true, silent = true, desc = "RUN CUSTOM COMMAND" })
--#endregion

-- Quit all, or leader+qq (comes from lazyvim)
vim.cmd([[command! Q qall]])

vim.keymap.set("n", "<C-n>", "<cmd>Neotree toggle<CR>", { desc = "Neotree Toggle" })

-- Diagnostics
vim.keymap.set("n", "I", vim.diagnostic.open_float, { desc = "Line Diagnostics", silent = true })

-- Save file
vim.keymap.set({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save File", silent = true })
-- vim.api.nvim_set_keymap("n", "<C-s>", ":w<CR>", { noremap = true, silent = true })
-- vim.api.nvim_set_keymap("i", "<C-s>", "<C-o>:w<CR>", { noremap = true, silent = true })

-- Autosave toggle
vim.keymap.set("n", "<leader>as", ":ASToggle<CR>", {})
-- GitSigns toggle
vim.keymap.set("n", "<leader>ug", ":Gitsigns toggle_signs<CR>", {})

-- ZenMode
vim.api.nvim_set_keymap("n", "<leader>zz", "<CMD>ZenMode<CR>", { desc = "ZenMode", silent = false })

-- Split windows
vim.keymap.set("n", "<Leader>w'", "<cmd>split<CR>")
vim.keymap.set("n", '<Leader>w"', "<cmd>vs<CR>")
vim.keymap.set("n", "<Leader>wq", "<cmd>q<CR>")

-- Close
-- vim.keymap.set("n", "<Leader>wq", "<cmd>bdelete<CR>")
vim.keymap.set("n", "<Leader>x", "<cmd>bdelete<CR>")
-- vim.keymap.set("n", "<Leader>wq", "<cmd>wq<CR>")
-- vim.keymap.set("n", "<Leader>wx", "<cmd>q<CR>")

-- Unmap Ctrl+/
-- vim.api.nvim_del_keymap("n", "<C-/>")
-- vim.api.nvim_del_keymap("n", "<C-_>") -- This is for the alternative keybinding that may map to the same key

-- floating terminal
local map = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

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

-- Hover documentation
vim.api.nvim_set_keymap("n", "<C-I>", ":lua vim.lsp.buf.hover()<CR>", { noremap = true, silent = true })

-- Symbol references
vim.api.nvim_set_keymap("n", "<leader>r", ":lua vim.lsp.buf.references()<CR>", { noremap = true, silent = true })

-- quick fix navigation, not sure wtf this is
-- vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
-- vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")

-- change every occurance of the word under cursor
-- vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
