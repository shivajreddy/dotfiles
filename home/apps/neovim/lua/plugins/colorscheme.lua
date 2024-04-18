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
			flavour = "auto", -- latte, frappe, macchiato, mocha
			background = { -- :h background
				light = "mocha",
				dark = "mocha",
			},
			transparent_background = false, -- disables setting the background color.
			show_end_of_buffer = false, -- shows the '~' characters after the end of buffers
			term_colors = false, -- sets terminal colors (e.g. `g:terminal_color_0`)
			dim_inactive = {
				enabled = false, -- dims the background color of inactive window
				shade = "dark",
				percentage = 0.15, -- percentage of the shade to apply to the inactive window
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
					-- BufferLine
					buffer_selected = { bg = "#b4befe", fg = "#11111b" },
					buffer_visible = { bg = "#b4befe", fg = "#11111b" },
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
			italic = true,
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
	},

	-- Configure LazyVim to load the colorscheme
	{
		"LazyVim/LazyVim",
		opts = {
			colorscheme = "catppuccin",
			-- colorscheme = "rose-pine-main", -- rose-pine-main rose-pine-moon  rose-pine-dawn
			-- colorscheme = "tokyonight",
		},
	},
}
