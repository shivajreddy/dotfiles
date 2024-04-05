return {
	"folke/noice.nvim",
	opts = {

		lsp = {
			override = {
				["vim.lsp.util.convert_input_to_markdown_lines"] = true,
				["vim.lsp.util.stylize_markdown"] = true,
				["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
			},
		},

		presets = { bottom_search = true },

		--[[
		views = {
			cmdline_popup = {
				border = {
					style = "rounded",
					padding = { 1, 1 },
				},
				filter_options = {},
				win_options = {
					winhighlight = "NormalFloat:NormalFloat,FloatBorder:FloatBorder",
				},
			},
		},
    --]]

		popupmenu = {
			enabled = true,
			backend = "nui",
			kind_icons = false,
		},

		format = {
			cmdline = { pattern = "^:", icon = " ", lang = "vim" },
			search_down = { kind = "search", pattern = "^/", icon = " 🔍 " },
			search_up = { kind = "search", pattern = "^%?", icon = " 🔍 ", lang = "regex" },
			filter = { pattern = "^:%s*!", icon = "$", lang = "bash" },
			lua = { pattern = { "^:%s*lua%s+", "^:%s*lua%s*=%s*", "^:%s*=%s*" }, icon = "", lang = "lua" },
			help = { pattern = "^:%s*he?l?p?%s+", icon = "?" },
			input = {}, -- Used by input()
		},
	},
}

--[[
return {
	"folke/noice.nvim",
	event = "VeryLazy",
	opts = {
		routes = {
			{
				filter = { event = "notify", find = "No information available" },
				opts = { skip = true },
			},
		},
		presets = {
			lsp_doc_border = true,
		},
	},
	dependencies = {
		"MunifTanjim/nui.nvim",
		"rcarriga/nvim-notify",
	},
}
--]]

--[[
  plugins = {
    noice = {
      enable = true;
      presets = {
        bottom_search = true;
      };
      cmdline.format = {
        cmdline = {icon = ">";};
        search_down = {icon = "🔍⌄";};
        search_up = {icon = "🔍⌃";};
        filter = {icon = "$";};
        lua = {icon = "☾";};
        help = {icon = "?";};
      };
      format = {
        level = {
          icons = {
            error = "✖";
            warn = "▼";
            info = "●";
          };
        };
      };
      popupmenu = {
        kindIcons = false;
      };
      extraOptions = {
        inc_rename.cmdline.format.IncRename = {icon = "⟳";};
      };
    };
  };
  extraConfigLua = ''
    -- Noice recommended config
    require("noice").setup({
    		lsp = {
    		override = {
    		["vim.lsp.util.convert_input_to_markdown_lines"] = true,
    		["vim.lsp.util.stylize_markdown"] = true,
    		["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
    		},
    		},
    		})
  '';
-]]
