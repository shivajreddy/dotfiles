-- MY Lazy plugin manager settings
local telescope_plugin = require("shiva.plugins.telescope")
local lsp_setup = require("shiva.plugins.lsp")
local theme = require("shiva.plugins.theme")
local neotree = require("shiva.plugins.neotree")
local lualine = require("shiva.plugins.lualine")

-- [[ Install `lazy.nvim` plugin manager ]]
--    See `:help lazy.nvim.txt` or https://github.com/folke/lazy.nvim for more info
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
  vim.fn.system { 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath }
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)


all_plugins = {
	-- NOTE: Plugins can be added with a link (or for a github repo: 'owner/repo' link).
	'tpope/vim-sleuth', -- Detect tabstop and shiftwidth automatically

	-- NOTE: Plugins can also be added by using a table,
	-- with the first argument being the link and the following
	-- keys can be used to configure plugin behavior/loading/etc.
	--
	-- Use `opts = {}` to force a plugin to be loaded.
	--
	--  This is equivalent to:
	--    require('Comment').setup({})

	-- "gc" to comment visual regions/lines
	{ 'numToStr/Comment.nvim', opts = {
    padding = true,
    toggler = {
      line = '<leader>/',
      block = '<leader>b/'
    },
  } },

	-- Here is a more advanced example where we pass configuration
	-- options to `gitsigns.nvim`. This is equivalent to the following lua:
	--    require('gitsigns').setup({ ... })
	--
	-- See `:help gitsigns` to understand what the configuration keys do
	-- Adds git related signs to the gutter, as well as utilities for managing changes
	{ 'lewis6991/gitsigns.nvim', opts = {
		signs = {
			add = { text = '+' },
			change = { text = '~' },
			delete = { text = '_' },
			topdelete = { text = '‾' },
			changedelete = { text = '~' },
		},
	}, },

	-- NOTE: Plugins can also be configured to run lua code when they are loaded.
	--
	-- This is often very useful to both group configuration, as well as handle
	-- lazy loading plugins that don't need to be loaded immediately at startup.
	--
	-- For example, in the following configuration, we use:
	--  event = 'VimEnter'
	--
	-- which loads which-key before all the UI elements are loaded. Events can be
	-- normal autocommands events (`:help autocmd-events`).
	--
	-- Then, because we use the `config` key, the configuration only runs
	-- after the plugin has been loaded:
	--  config = function() ... end

	-- Useful plugin to show you pending keybinds.
	{ 'folke/which-key.nvim',
	event = 'VimEnter', -- Sets the loading event to 'VimEnter'
	config = function() -- This is the function that runs, AFTER loading
		require('which-key').setup()

		-- Document existing key chains
		require('which-key').register {
			['<leader>c'] = { name = '[C]ode', _ = 'which_key_ignore' },
			['<leader>d'] = { name = '[D]ocument', _ = 'which_key_ignore' },
			['<leader>r'] = { name = '[R]ename', _ = 'which_key_ignore' },
			['<leader>s'] = { name = '[S]earch', _ = 'which_key_ignore' },
			['<leader>w'] = { name = '[W]orkspace', _ = 'which_key_ignore' },
		}
	end, },

	-- Telescope has its own file
	telescope_plugin,

	-- Set up LSP
	lsp_setup,

	-- Theme
	theme,

  -- Neotree
  {'MunifTanjim/nui.nvim'},
  neotree,


  lualine,

	-- Highlight todo, notes, etc in comments
	{ 'folke/todo-comments.nvim', event = 'VimEnter', dependencies = { 'nvim-lua/plenary.nvim' }, opts = { signs = false } },



	  { -- Highlight, edit, and navigate code
	  'nvim-treesitter/nvim-treesitter',
	  build = ':TSUpdate',
	  config = function()
		  -- [[ Configure Treesitter ]] See `:help nvim-treesitter`

		  ---@diagnostic disable-next-line: missing-fields
		  require('nvim-treesitter.configs').setup {
			  ensure_installed = { 'bash', 'c', 'html', 'lua', 'markdown', 'vim', 'vimdoc' },
			  -- Autoinstall languages that are not installed
			  auto_install = true,
			  highlight = { enable = true },
			  indent = { enable = true },
		  }

		  -- There are additional nvim-treesitter modules that you can use to interact
		  -- with nvim-treesitter. You should go explore a few and see what interests you:
		  --
		  --    - Incremental selection: Included, see `:help nvim-treesitter-incremental-selection-mod`
		  --    - Show your current context: https://github.com/nvim-treesitter/nvim-treesitter-context
		  --    - Treesitter + textobjects: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
	  end,
	  },

}

--vim.cmd.colorscheme "catppuccin"
--
local ui = {
    -- If you have a Nerd Font, set icons to an empty table which will use the
    -- default lazy.nvim defined Nerd Font icons otherwise define a unicode icons table
    icons = vim.g.have_nerd_font and {} or {
      cmd = '⌘',
      config = '🛠',
      event = '📅',
      ft = '📂',
      init = '⚙',
      keys = '🗝',
      plugin = '🔌',
      runtime = '💻',
      require = '🌙',
      source = '📄',
      start = '🚀',
      task = '📌',
      lazy = '💤 ',
    },
}

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et

require("lazy").setup(all_plugins, ui, opts)


