-- ALL my basic keymaps, not plugin specific
-- [[ Basic Keymaps ]]
--  See `:help vim.keymap.set()`


-- Set highlight on search, but clear on pressing <Esc> in normal mode
vim.opt.hlsearch = true
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous [D]iagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next [D]iagnostic message' })

vim.api.nvim_set_keymap('n', '<leader>q', ':q<CR>', { noremap = true, silent = true, desc = 'Quit' })
vim.api.nvim_set_keymap('n', '<leader>wq', ':wq<CR>', { noremap = true, silent = true, desc = 'Write Quit'})
vim.api.nvim_set_keymap('n', '<leader>s', ':w<CR>', { noremap = true, silent = true, desc = 'Save' })

vim.api.nvim_set_keymap('n', '<C-u>', '<C-u> zz<CR>', {noremap = true, silent = true, desc = 'Scroll Up'})
vim.api.nvim_set_keymap('n', '<C-d>', '<C-d> zz<CR>', {noremap = true, silent = true, desc = 'Scroll Up'})

--vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Show diagnostic [E]rror messages' })
--vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })

-- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
-- is not what someone will guess without a bit more experience.
--
-- NOTE: This won't work in all terminal emulators/tmux/etc. Try your own mapping
-- or just use <C-\><C-n> to exit terminal mode
vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })



_G.toggle_rn = function ()
  if vim.wo.relativenumber then
    vim.wo.relativenumber = false
  else
    vim.wo.relativenumber = true
  end
end

_G.toggle_n = function ()
  if vim.wo.number then
    vim.wo.relativenumber = false
    vim.wo.number = false
  else
    vim.wo.number = true
    vim.wo.relativenumber = true
  end
end

vim.api.nvim_set_keymap('n', '<Leader>rn', ':lua toggle_rn()<CR>', 
 {noremap = true, silent = true, desc = 'Toggle Relative Number'}
)

vim.api.nvim_set_keymap('n', '<Leader>n', ':lua toggle_n()<CR>', 
 {noremap = true, silent = true, desc = 'Toggle Number'}
)


-- vim.api.nvim_set_keymap('n', '', '', {noremap = true, silent = true, desc = ''})

-- vim.api.nvim_set_keymap('n', '<Leader>rn', ':relative number', {noremap = true, silent = true, desc = 'Toggle Relative Number'})


-- TIP: Disable arrow keys in normal mode
-- vim.keymap.set('n', '<left>', '<cmd>echo "Use h to move!!"<CR>')
-- vim.keymap.set('n', '<right>', '<cmd>echo "Use l to move!!"<CR>')
-- vim.keymap.set('n', '<up>', '<cmd>echo "Use k to move!!"<CR>')
-- vim.keymap.set('n', '<down>', '<cmd>echo "Use j to move!!"<CR>')

-- Keybinds to make split navigation easier.
--  Use CTRL+<hjkl> to switch between windows
--
--  See `:help wincmd` for a list of all window commands
vim.keymap.set('n', '<Left>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
vim.keymap.set('n', '<Right>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
vim.keymap.set('n', '<Down>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
vim.keymap.set('n', '<Up>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

-- [[ Basic Autocommands ]]
--  See `:help lua-guide-autocommands`

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})




