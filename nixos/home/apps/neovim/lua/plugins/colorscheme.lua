return {

	-- TokyoNight
	{
		"folke/tokyonight.nvim",
		lazy = false,
		-- opts = { style = "moon" },
		opts = { style = "night" },
	},

	--  catppuccin
	{
		"catppuccin/nvim",
		lazy = false,
		name = "catppuccin",
		opts = {
			flavour = "mocha", -- latte, frappe, macchiato, mocha
			background = { -- :h background
				light = "mocha",
				dark = "mocha",
			},
			transparent_background = true, -- disables setting the background color.
			show_end_of_buffer = false, -- shows the '~' characters after the end of buffers
			term_colors = false, -- sets terminal colors (e.g. `g:terminal_color_0`)
			dim_inactive = {
				enabled = false, -- dims the background color of inactive window
				shade = "dark",
				percentage = 0.85, -- percentage of the shade to apply to the inactive window
			},
			no_italic = false, -- Force no italic
			no_bold = false, -- Force no bold
			no_underline = false, -- Force no underline
			styles = { -- Handles the styles of general hi groups (see `:h highlight-args`):
				comments = { "italic" }, -- Change the style of comments
				conditionals = { "italic" },
				loops = {},
				functions = {},
				keywords = {},
				strings = {},
				variables = {},
				numbers = {},
				booleans = {},
				properties = {},
				types = {},
				operators = {},
				-- miscs = {}, -- Uncomment to turn off hard-coded styles
			},
			color_overrides = {
				mocha = {
					base = "#181825",
					-- base = "#11111b",
				},
			},
			custom_highlights = function()
				return {
					-- NeoTree
					NeoTreeNormal = { bg = "#11111b" },
					NeoTreeNormalNC = { bg = "#11111b" },
				}
			end,
			default_integrations = true,
			integrations = {
				aerial = true,
				alpha = true,
				cmp = true,
				dashboard = true,
				flash = true,
				gitsigns = true,
				headlines = true,
				illuminate = true,
				indent_blankline = { enabled = true },
				leap = true,
				lsp_trouble = true,
				mason = true,
				markdown = true,
				mini = {
					enabled = true,
					indentscope_color = "",
				},
				native_lsp = {
					enabled = true,
					underlines = {
						errors = { "undercurl" },
						hints = { "undercurl" },
						warnings = { "undercurl" },
						information = { "undercurl" },
					},
				},
				navic = { enabled = true, custom_bg = "lualine" },
				neotest = true,
				-- neotree = true,
				neotree = { enabled = true, active_bg = "#11111b" },
				-- neotree = { enabled = true, active_bg = "#181825" },
				noice = true,
				notify = true,
				nvimtree = true,
				semantic_tokens = true,
				telescope = true,
				treesitter = true,
				treesitter_context = true,
				which_key = true,
			},
		},
	},

	-- Rose Pine
	-- Simple Config
	-- { "rose-pine/neovim", name = "rose-pine", styles = { transparency = true } },
	-- Advanced Config
	{
		"rose-pine/neovim",
		lazy = false,
		name = "rose-pine",
		variant = "main", -- auto, main, moon, or dawn
		dark_variant = "main", -- main, moon, or dawn
		light_variant = "main",
		dim_inactive_windows = true,
		extend_background_behind_borders = true,

		enable = {
			terminal = true,
			legacy_highlights = true, -- Improve compatibility for previous versions of Neovim
			migrations = true, -- Handle deprecated options automatically
		},

		styles = {
			bold = true,
			italic = false,
			transparency = true,
		},

		groups = {
			border = "muted",
			link = "iris",
			panel = "surface",

			error = "love",
			hint = "iris",
			info = "foam",
			note = "pine",
			todo = "rose",
			warn = "gold",

			git_add = "foam",
			git_change = "rose",
			git_delete = "love",
			git_dirty = "rose",
			git_ignore = "muted",
			git_merge = "iris",
			git_rename = "pine",
			git_stage = "iris",
			git_text = "rose",
			git_untracked = "subtle",

			h1 = "iris",
			h2 = "foam",
			h3 = "rose",
			h4 = "gold",
			h5 = "pine",
			h6 = "foam",
		},

		highlight_groups = {

			-- Comment = { fg = "foam" },
			-- VertSplit = { fg = "muted", bg = "muted" },
			--
			-- StatusLine = { fg = "love", bg = "love", blend = 10 },
			-- StatusLineNC = { fg = "subtle", bg = "surface" },

			-- Telescope
			TelescopeBorder = { fg = "highlight_high", bg = "none" },
			TelescopeNormal = { bg = "none" },
			TelescopePromptNormal = { bg = "base" },
			TelescopeSelection = { fg = "text", bg = "base" },
			TelescopeSelectionCaret = { fg = "rose", bg = "rose" },
			TelescopeResultsNormal = { fg = "subtle", bg = "none" },

			-- TelescopeBorder = { fg = "overlay", bg = "overlay" },
			-- TelescopeNormal = { fg = "subtle", bg = "overlay" },
			-- TelescopeSelection = { fg = "text", bg = "highlight_med" },
			-- TelescopeSelectionCaret = { fg = "love", bg = "highlight_med" },
			TelescopeMultiSelection = { fg = "text", bg = "highlight_high" },

			TelescopeTitle = { fg = "base", bg = "love" },
			TelescopePromptTitle = { fg = "base", bg = "pine" },
			TelescopePreviewTitle = { fg = "base", bg = "iris" },

			-- TelescopePromptNormal = { fg = "text", bg = "surface" },
			TelescopePromptBorder = { fg = "surface", bg = "surface" },
		},
	},

	-- :: GRUVBOX ::
	{
		"ellisonleao/gruvbox.nvim",
		lazy = false,
		config = true,
		opts = {
			terminal_colors = true, -- add neovim terminal colors
			undercurl = true,
			underline = true,
			bold = true,
			italic = {
				strings = true,
				emphasis = true,
				comments = true,
				operators = false,
				folds = true,
			},
			strikethrough = true,
			invert_selection = false,
			invert_signs = false,
			invert_tabline = false,
			invert_intend_guides = false,
			inverse = true, -- invert background for search, diffs, statuslines and errors
			contrast = "", -- can be "hard", "soft" or empty string
			palette_overrides = {},
			overrides = {},
			dim_inactive = false,
			transparent_mode = false,
		},
	},

	-- Configure LazyVim to load the colorscheme
	{
		"LazyVim/LazyVim",
		opts = {
			-- colorscheme = "catppuccin",
			-- colorscheme = "rose-pine-main", -- rose-pine-main rose-pine-moon  rose-pine-dawn
			colorscheme = "gruvbox",
			-- colorscheme = "tokyonight",
		},
	},
}
