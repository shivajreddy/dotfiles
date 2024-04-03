{pkgs, ...}: {
  colorschemes = {
    catppuccin = {
      enable = true;
      integrations = {
        aerial = true;
        alpha = true;
        cmp = true;
        dashboard = true;
        gitsigns = true;
        headlines = true;
        indent_blankline.enabled = true;
        leap = true;
        lsp_trouble = true;
        mason = true;
        mini.enabled = true;
        native_lsp = {
          enabled = true;
          underlines = {
            errors = ["undercurl"];
            hints = ["undercurl"];
            warnings = ["undercurl"];
            information = ["undercurl"];
          };
        };
        navic = {
          enabled = true;
          custom_bg = "lualine";
        };
        neotest = true;
        neotree = true;
        noice = true;
        notify = true;
        semantic_tokens = true;
        telescope.enabled = true;
        treesitter = true;
        treesitter_context = true;
        which_key = true;
      };
    };
  };

  /*
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
    */
}
