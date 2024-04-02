{pkgs, ...}: {
  colorschemes = {
    catppuccin = {
      enable = true;
      integrations = {
        aerial = true;
        alpha = true;
        cmp = true;
        dashboard = true;
        flash = true;
        gitsigns = true;
        headlines = true;
        indent_blankline.enabled = true;
      };
    };
    # tokyonight.enable = true;
    # flavor = "mocha";
  };

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
}
