-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua

--#region CPP, build & run & paste into output.txt
--[[
vim.keymap.set("n", "<F8>", function()
  vim.cmd("write")
  if vim.fn.expand("%:e") ~= "cpp" then
    vim.notify("Not a C++ file!", vim.log.levels.ERROR)
    return
  end

  local filepath = vim.fn.expand("%:p")
  local dir = vim.fn.expand("%:p:h")
  local input_path = dir .. "/input.txt"
  local output_path = dir .. "/output.txt"

  local cmd = string.format("g++ '%s' -o out && ./out < '%s' > '%s'", filepath, input_path, output_path)

  vim.fn.jobstart(cmd, {
    cwd = dir,
    stdout_buffered = true,
    stderr_buffered = true,
    on_exit = function(_, code)
      if code == 0 then
        vim.notify("Output written to output.txt", vim.log.levels.INFO)
      else
        vim.notify("Build or execution failed", vim.log.levels.ERROR)
      end
    end,
  })
end, { noremap = true, silent = true, desc = "CPP BUILD & RUN to output.txt" })
--]]
--#endregion

--#region CPP, build & run
vim.keymap.set("n", "<F8>", function()
  vim.cmd("write") -- save the current buffer
  -- vim.notify("Running C++ build & run", vim.log.levels.INFO)
  local filename = vim.fn.expand("%:t:r") -- Get filename without extension
  local filepath = vim.fn.expand("%:p") -- Get full path
  if vim.fn.expand("%:e") ~= "cpp" then
    vim.notify("Not a C++ file!", vim.log.levels.ERROR)
    return
  end
  -- local cmd = string.format("g++ '%s' -o '%s' && ./'%s'", filepath, filename, filename)
  -- local cmd = string.format("g++ '%s' -o '%s' && ./'%s' < input.txt", filepath, filename, filename)
  local cmd = string.format("g++ -std=c++17 -Wno-cpp '%s' -o out && ./out < input.txt", filepath)
  -- local cmd = string.format( "g++ -std=c++17 -O2 -Wall -Wextra -Wshadow -Wconversion -Wfloat-equal -Wno-sign-conversion -DONLINE_JUDGE -g '%s' -o out && ./out < input.txt", filepath)

  -- Using toggleterm
  local Terminal = require("toggleterm.terminal").Terminal
  local cpp_term = Terminal:new({
    cmd = cmd,
    dir = vim.fn.expand("%:p:h"), -- set terminal's cwd to current file's folder
    direction = "float", -- horizontal, vertical, tab, float
    float_opts = {
      border = "double",
      width = function()
        return math.floor(vim.o.columns * 0.8)
      end,
      height = function()
        return math.floor(vim.o.lines * 0.8)
      end,
    },
    close_on_exit = false,
  })
  cpp_term:toggle()
end, { noremap = true, silent = true, desc = "CPP BUILD & RUN" })
--[[
--]]
--#endregion

--[[
vim.keymap.set("n", "<F5>", function()
  -- opens the floating terminal
  Snacks.terminal(nil, { cwd = LazyVim.root() })
  -- Wait for the terminal to open (you might need to adjust the delay)
  vim.defer_fn(function()
    -- Type the command and press Enter
    -- local cmd = "clear && ./build.ps1 && ./build/out.exe"  -- for powershell script
    local cmd = "cls && .\\build.bat " -- for cmd prompt script
    -- local cmd = "go run ." -- for cmd prompt script
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(cmd .. "<CR>", true, true, true), "n", false)
  end, 100) -- Adjust the delay (in milliseconds) if necessary
end, { noremap = true, silent = true, desc = "RUN CUSTOM COMMAND" })

--#region keymap to run a custom command
vim.keymap.set("n", "<F6>", function()
  -- Get the current file's directory
  local current_file = vim.fn.expand("%:p:h")

  -- Change to the directory of the current file
  vim.cmd("cd " .. current_file)

  -- Command to run in the terminal
  local command = "build.bat"

  -- map("n", "<c-/>",      function() Snacks.terminal(nil, { cwd = LazyVim.root() }) end, { desc = "Terminal (Root Dir)" })
  Snacks.terminal(nil, { cwd = LazyVim.root() })
  -- end, { desc = "Terminal (Root Dir)" })
end, { noremap = true, silent = true, desc = "RUN CUSTOM COMMAND" })
--#endregion
--]]

-- Jump between matching brackets with tab, (default % will still work)
vim.keymap.set("n", "<Tab>", "%", { noremap = true, silent = true, desc = "Jump to matching bracket" })

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
-- Quit all, or leader+qq (comes from lazyvim)
vim.cmd([[command! Q qall]])

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

-- open the current word in man pages
-- Function to open man page for the word under the cursor
local function open_man_page()
  local word = vim.fn.expand("<cword>") -- Get the word under the cursor
  vim.cmd("Man " .. word) -- Open the man page for the word
end

-- Create a custom command
vim.api.nvim_create_user_command("ManUnderCursor", open_man_page, {})

-- Optionally, create a key mapping
vim.keymap.set(
  "n",
  "<leader>m",
  open_man_page,
  { noremap = true, silent = true, desc = "Open man page for word under cursor" }
)

-- Comment toggle
vim.keymap.set("n", "<leader>/", "gcc", { desc = "Comment toggle current line", remap = true })
vim.keymap.set("v", "<leader>/", "gc", { desc = "Comment toggle selection", remap = true })
