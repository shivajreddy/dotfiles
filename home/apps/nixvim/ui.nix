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

    indent-blankline = {
      enable = true;
      settings = {
        indent = {
          char = "│";
          tab_char = "│";
        };
        scope = {
          enabled = false;
        };
      };
    };

    bufferline = {
      enable = true;
      alwaysShowBufferline = false;
    };

    which-key.enable = true;

    # this is not yet there in NIXVIM, have to do manually
    # nvim-web-devicons = true;

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
  };
}
