return {
	"folke/zen-mode.nvim",
	lazy = false,
	opts = {
		window = {
			backdrop = 1, -- shade the backdrop of the Zen window. Set to 1 to keep the same as Normal
			-- height and width can be:
			-- * an absolute number of cells when > 1
			-- * a percentage of the width / height of the editor when <= 1
			-- * a function that returns the width or the height
			width = 120, -- width of the Zen window
			height = 120, -- height of the Zen window
			-- by default, no options are changed for the Zen window
			-- uncomment any of the options below, or add other vim.wo options you want to apply
			options = {
				signcolumn = "no", -- disable signcolumn
				number = false, -- disable number column
				relativenumber = false, -- disable relative numbers
				cursorline = false, -- disable cursorline
				cursorcolumn = false, -- disable cursor column
				foldcolumn = "0", -- disable fold column
				list = false, -- disable whitespace characters
			},
		},
		plugins = {
			-- disable some global vim options (vim.o...)
			-- comment the lines to not apply the options
			options = {
				enabled = true,
				ruler = false, -- disables the ruler text in the cmd line area
				showcmd = false, -- disables the command in the last line of the screen
				-- you may turn on/off statusline in zen mode by setting 'laststatus'
				-- statusline will be shown only if 'laststatus' == 3
				laststatus = 0, -- turn off the statusline in zen mode
			},
			twilight = { enabled = false }, -- enable to start Twilight when zen mode opens
			gitsigns = { enabled = false }, -- disables git signs
			tmux = { enabled = false }, -- disables the tmux statusline when enabled = false
			-- this will change the font size on kitty when in zen mode
			-- to make this work, you need to set the following kitty options:
			-- - allow_remote_control socket-only
			-- - listen_on unix:/tmp/kitty
			kitty = {
				-- using a dedicated kitty-config and i load it using the on_open function below
				enabled = false,
				font = "+4", -- font size increment
			},
			-- this will change the font size on alacritty when in zen mode
			-- requires  Alacritty Version 0.10.0 or higher
			-- uses `alacritty msg` subcommand to change font size
			alacritty = {
				enabled = false,
				font = "14", -- font size
			},
			-- this will change the font size on wezterm when in zen mode
			-- See alse also the Plugins/Wezterm section in this projects README
			wezterm = {
				enabled = false,
				-- can be either an absolute font size or the number of incremental steps
				font = "+4", -- (10% increase per step)
			},
		},
		-- callback where you can add custom code when the Zen window opens
		on_open = function(win)
			vim.fn.system([[tmux set status off]])
			-- vim.fn.system([[tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z]])
			-- NOTE: i got the below setting from https://sw.kovidgoyal.net/kitty/generated/rc/#load-config
			vim.fn.system([[kitty @ load-config ~/dotfiles/home/apps/terminal/kitty/obsidian_kitty.conf]])
		end,
		-- callback where you can add custom code when the Zen window closes
		on_close = function()
			vim.fn.system([[tmux set status on]])
			vim.fn.system([[kitty @ load-config ignore_overrides]])
			vim.fn.system([[kitty @ load-config ~/dotfiles/home/apps/terminal/kitty/kitty.conf]])
			-- vim.fn.system([[tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z]])
		end,
	},
}
