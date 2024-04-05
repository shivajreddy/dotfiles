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

		presets = { bottom_search = true, command_palette = false },

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
