{
  pkgs,
  helpers,
  ...
}: {
  plugins = {
    comment = {
      enable = true;
      settings = {
        toggler = {
          line = "<leader>/";
          block = "<C-/>";
        };
      };
    };

    # nvim-notify
    notify = {
      enable = true;
      stages = "static";
      timeout = 3000;
      maxHeight = helpers.mkRaw ''
        function()
        return math.floor(vim.o.lines * 0.75)
        end
      '';
      maxWidth = helpers.mkRaw ''
        function()
        	return math.floor(vim.o.columns * 0.75)
        end
      '';
      onOpen = ''
        function(win)
        		vim.api.nvim_win_set_config(win, { zindex = 100 })
        		end
      '';
    };

    # bufferline
    bufferline = {
      enable = true;
      closeCommand = helpers.mkRaw ''
        function(n) require("mini.bufremove").delete(n, false) end
      '';
      rightMouseCommand = helpers.mkRaw ''
        function(n) require("mini.bufremove").delete(n, false) end
      '';
      diagnostics = "nvim_lsp";
      alwaysShowBufferline = false;
      diagnosticsIndicator = ''
        function(_, _, diag)
           local icons = require("lazyvim.config").icons.diagnostics
           local ret = (diag.error and icons.Error .. diag.error .. " " or "")
             .. (diag.warning and icons.Warn .. diag.warning or "")
           return vim.trim(ret)
         end
      '';
      offsets = [
        {
          filetype = "neo-tree";
          text = "Neo-tree";
          highlight = "Directory";
          text_align = "left";
        }
      ];
    };

    lazy.enable = true;

    # lualine -> Status Line
    lualine = {
      enable = true;
      iconsEnabled = true;
      theme = "catppuccin";
      globalstatus = true;
      disabledFiletypes = {
        statusline = ["dashboard" "alpha" "starter"];
      };
      sections = {
        lualine_a = ["mode"];
        lualine_b = ["branch"];
        lualine_c = [
          /*
             root_dir
                 {
                   name = helpers.mkRaw ''
                     vim.fn.expand('%:p:h')
                   '';
                 }
          #
          */
          {
            name = "filetype";
            icons_enabled = true;
            fmt = "";
            separator = {left = "";};
            padding = {
              left = 1;
              right = 0;
            };
          }
          /*
                    pretty_path
                 {
                   name = helpers.mkRaw ''
                               function pretty_path(opts)
                               opts = vim.tbl_extend("force", {
                               	relative =  "cwd",
                               	modified_hl = "matchParen",
                               	directory_hl = "",
                               	filename_hl = "Bold",
                               	modified_sign = "",
                               }, opts or {})
                        				return "hi"
                     end
              pretty_path()
                   '';
                 }
          #
          */
        ];
      };
      extensions = ["neo-tree" "lazy"];
    };

    # indent guides for neovim
    indent-blankline = {
      enable = true;
      settings = {
        indent = {
          char = "│";
          tab_char = "│";
        };
        scope.enabled = false;
        exclude = {
          buftypes = [
            "terminal"
            "quickfix"
          ];
          filetypes = [
            "help"
            "alpha"
            "dashboard"
            "neo-tree"
            "Trouble"
            "trouble"
            "lazy"
            "notify"
            "toggleterm"
            "lazyterm"
          ];
        };
      };
    };

    # Active indent guide and indext text objects
    mini = {
      enable = true;
      modules = {
        surround = {};
        indentscope = {
          symbol = "│";
          options = {try_as_border = true;};
        };
      };
    };

    which-key.enable = true;

    dashboard = {
      enable = true;
      header = [
        "███╗   ██╗██╗██╗  ██╗██╗   ██╗██╗███╗   ███╗"
        "████╗  ██║██║╚██╗██╔╝██║   ██║██║████╗ ████║"
        "██╔██╗ ██║██║ ╚███╔╝ ██║   ██║██║██╔████╔██║"
        "██║╚██╗██║██║ ██╔██╗ ╚██╗ ██╔╝██║██║╚██╔╝██║"
        "██║ ╚████║██║██╔╝ ██╗ ╚████╔╝ ██║██║ ╚═╝ ██║"
        "╚═╝  ╚═══╝╚═╝╚═╝  ╚═╝  ╚═══╝  ╚═╝╚═╝     ╚═╝"
      ];
      center = [
        {
          action = "Telescope oldfiles";
          desc = "Recent Files";
          icon = "";
          shortcut = "r";
        }
        {
          action = "Telescope live_grep";
          desc = " Find File";
          icon = " ";
          shortcut = "f";
        }
        {
          action = "qa";
          desc = " Quit";
          icon = " ";
          shortcut = "q";
        }
      ];
    };

    /*
    alpha = {
      enable = true;
      # theme = "dashboard";
      layout = [
        {
          type = "padding";
          val = 2;
        }
        {
          opts = {
            hl = "Type";
            position = "center";
          };
          type = "text";
          val = [
            "███╗   ██╗██╗██╗  ██╗██╗   ██╗██╗███╗   ███╗"
            "████╗  ██║██║╚██╗██╔╝██║   ██║██║████╗ ████║"
            "██╔██╗ ██║██║ ╚███╔╝ ██║   ██║██║██╔████╔██║"
            "██║╚██╗██║██║ ██╔██╗ ╚██╗ ██╔╝██║██║╚██╔╝██║"
            "██║ ╚████║██║██╔╝ ██╗ ╚████╔╝ ██║██║ ╚═╝ ██║"
            "╚═╝  ╚═══╝╚═╝╚═╝  ╚═╝  ╚═══╝  ╚═╝╚═╝     ╚═╝"
          ];
        }
        {
          type = "padding";
          val = 2;
        }
        {
          type = "group";
          val = [
            {
              on_press = {
                __raw = "function() vim.cmd[[ene]] end";
              };
              opts = {
                shortcut = "n";
              };
              type = "button";
              val = "  New file";
            }
            {
              on_press = {
                __raw = "function() vim.cmd[[qa]] end";
              };
              opts = {
                shortcut = "q";
              };
              type = "button";
              val = "󰗼 Quit Neovim";
            }
          ];
        }
        {
          type = "padding";
          val = 2;
        }
        {
          opts = {
            hl = "Keyword";
            position = "center";
          };
          type = "text";
          val = "Inspiring quote here.";
        }
      ];
    };
    */
  };

  /*
  extraConfigLua = ''
    -- LuaLine Config
    	local icons = require("lazyvim.config").icons
      require('lualine').setup {
          sections = {
            lualine_c = {
              function() return LazyVim.lualine.root_dir() end,
              {
                "diagnostics",
                symbols = {
                  error = icons.diagnostics.Error,
                  warn = icons.diagnostics.Warn,
                  info = icons.diagnostics.Info,
                  hint = icons.diagnostics.Hint,
                },
              },
              { "filetype", icon_only = true, separator = "", padding = { left = 1, right = 0 } },
              function() return LazyVim.lualine.pretty_path() end,
            },
          },
        }
  '';
  #
  */
}
