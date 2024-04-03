{
  pkgs,
  helpers,
  ...
}: {
  keymaps = [
    # # # General
    # # Move Lines
    {
      mode = ["n"];
      key = "<A-j>";
      action = "<cmd>m .+1<cr>==";
      options = {
        desc = "Move Down";
        silent = true;
      };
    }
    {
      mode = ["n"];
      key = "<A-k>";
      action = "<cmd>m .-2<cr>==";
      options = {
        desc = "Move Up";
        silent = true;
      };
    }
    {
      mode = ["i"];
      key = "<A-j>";
      action = "<esc><cmd>m .+1<cr>==gi";
      options = {
        silent = true;
        desc = "Move Down";
      };
    }
    {
      mode = ["i"];
      key = "<A-k>";
      action = "<esc><cmd>m .-2<cr>==gi";
      options = {
        silent = true;
        desc = "Move Up";
      };
    }
    {
      mode = ["v"];
      key = "<A-j>";
      action = ":m '>+1<cr>gv=gv";
      options = {
        silent = true;
        desc = "Move Down";
      };
    }
    {
      mode = ["v"];
      key = "<A-k>";
      action = ":m '<-2<cr>gv=gv";
      options = {
        silent = true;
        desc = "Move Up";
      };
    }

    # # buffers
    {
      mode = "n";
      key = "<S-l>";
      action = "<cmd>BufferLineCycleNext<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "Next Buffer";
      };
    }
    {
      mode = "n";
      key = "<S-h>";
      action = "<cmd>BufferLineCyclePrev<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "Prev Buffer";
      };
    }
    {
      mode = "n";
      key = "<Leader>`";
      action = "<cmd>e #<CR>";
      options = {
        desc = "Switch to Other Buffer";
      };
    }

    # # Clear Search with <esc>
    {
      mode = "n";
      key = "<esc>";
      action = "<CMD>noh<CR><ESC>";
      options = {
        silent = true;
        noremap = true;
        desc = "Escape and Clear hlsearch";
      };
    }

    # # saner search behaviour of n and N
    {
      mode = "n";
      key = "n";
      action = "'Nn'[v:searchforward].'zv'";
      options = {
        expr = true;
        desc = "Next Search Result";
      };
    }
    {
      mode = "x";
      key = "n";
      action = "'Nn'[v:searchforward]";
      options = {
        expr = true;
        desc = "Next Search Result";
      };
    }
    {
      mode = "o";
      key = "n";
      action = "'Nn'[v:searchforward]";
      options = {
        expr = true;
        desc = "Next Search Result";
      };
    }
    {
      mode = "n";
      key = "N";
      action = "'nN'[v:searchforward].'zv'";
      options = {
        expr = true;
        desc = "Prev Search Result";
      };
    }
    {
      mode = "x";
      key = "N";
      action = "'nN'[v:searchforward]";
      options = {
        expr = true;
        desc = "Prev Search Result";
      };
    }
    {
      mode = "o";
      key = "N";
      action = "'nN'[v:searchforward]";
      options = {
        expr = true;
        desc = "Prev Search Result";
      };
    }

    # # Save File
    {
      mode = ["i" "x" "n" "s"];
      key = "<C-s>";
      action = "<cmd>w<cr><esc>";
      options.desc = "Save File";
    }

    # # keywordprg
    {
      mode = "n";
      key = "<leader>K";
      action = "<cmd>norm! K<cr>";
      options.desc = "Keywordprg";
    }

    # # better indenting
    {
      mode = "v";
      key = "<";
      action = "<gv";
    }
    {
      mode = "v";
      key = ">";
      action = ">gv";
    }

    # # formatting
    {
      mode = ["n" "v"];
      key = "<leader>mp";
      action = ":lua _G.format_with_conform()<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "[m]ake [p]retty by formatting";
      };
    }

    # # # PLUGINS - Category Specific Keymaps
    #	# UI
    # notify
    {
      mode = "n";
      key = "<Leader>un";
      action = helpers.mkRaw ''
        function()
        		require("notify").dismiss({ silent = true, pending = true })
        end
      '';
      options = {
        silent = true;
        noremap = true;
        desc = "Dismiss All Notifications";
      };
    }

    # # editor
    # neo-tree
    {
      mode = "n";
      key = "<C-n>";
      action = "<cmd>Neotree toggle<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "Toggle Neotree tree";
      };
    }
    {
      key = "<leader>ge";
      action = helpers.mkRaw ''
        function()
        	require("neo-tree.command").execute({source = 'git_status', toggle = true})
        	end
      '';
      options.desc = "Git Explorer";
    }
    {
      key = "<leader>be";
      action = helpers.mkRaw ''
        function()
        	require("neo-tree.command").execute({source = 'buffers', toggle = true})
        	end
      '';
      options.desc = "Buffer Explorer";
    }

    # flash
    {
      mode = ["n" "x" "o"];
    }

    {
      key = "<leader>u";
      mode = "n";
      action = "<cmd>UndotreeToggle<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "[u]ndotree toggle";
      };
    }
    {
      key = "<leader>gs";
      mode = "n";
      action = "<cmd>Git<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "[g]it [s]tatus";
      };
    }
    {
      key = "<C-y>";
      mode = "n";
      action = ":set cursorcolumn!<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "Toggle vertical column because [Y]AML sucks";
      };
    }
    {
      key = "J";
      mode = "n";
      action = "mzJ\`z"; # Keep cursor to the left
      options = {
        silent = true;
        noremap = true;
      };
    }
    {
      key = "<C-d>";
      mode = "n";
      action = "<C-d>zz"; # Keep cursor in middle
      options = {
        silent = true;
        noremap = true;
      };
    }
    {
      key = "<C-u>";
      mode = "n";
      action = "<C-u>zz"; # Keep cursor in middle
      options = {
        silent = true;
        noremap = true;
      };
    }
    {
      key = "N";
      mode = "n";
      action = "Nzzzv";
      options = {
        silent = true;
        noremap = true;
      };
    }
    {
      key = "<leader>p";
      mode = "x";
      action = "\"_dP";
      options = {
        silent = true;
        noremap = true;
        desc = "[p]reserve put";
      };
    }
    {
      key = "<leader>y";
      mode = ["n" "v"];
      action = "\"+y";
      options = {
        silent = true;
        noremap = true;
        desc = "[y]ank to system clipboard";
      };
    }
    {
      key = "<leader>Y";
      mode = "n";
      action = "\"+Y";
      options = {
        silent = true;
        noremap = true;
        desc = "[Y]ank line to system clipboard";
      };
    }
    {
      key = "<C-f>";
      mode = "n";
      action = "<cmd>!tmux neww tmux-sessionizer<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "[f]ind and switch tmux session";
      };
    }
    {
      key = "<C-k>";
      mode = "n";
      action = "<cmd>cnext<CR>zz";
      options = {
        silent = true;
        noremap = true;
        desc = "Next quickfix";
      };
    }
    {
      key = "<C-j>";
      mode = "n";
      action = "<cmd>cprev<CR>zz";
      options = {
        silent = true;
        noremap = true;
        desc = "Prev quickfix";
      };
    }
    {
      key = "<leader>k";
      mode = "n";
      action = "<cmd>lnext<CR>zz";
      options = {
        silent = true;
        noremap = true;
        desc = "Next quickfix location";
      };
    }
    {
      key = "<leader>j";
      mode = "n";
      action = "<cmd>lprev<CR>zz";
      options = {
        silent = true;
        noremap = true;
        desc = "Prev quickfix location";
      };
    }
    {
      key = "<leader>da";
      mode = "n";
      action = ":lua vim.lsp.buf.code_action()<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "[d]iagnostic changes [a]ccepted";
      };
    }

    # DAP Debugging
    {
      key = "<leader>b";
      mode = "n";
      action = ":lua require'dap'.toggle_breakpoint()<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "Toggle DAP [b]reakpoint";
      };
    }
    {
      key = "<leader>B";
      mode = "n";
      action = ":lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "Set DAP [B]reakpoint";
      };
    }
    {
      key = "<leader>dtg";
      mode = "n";
      action = ":lua require'dap-go'.debug_test()<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "DAP [d]ebug [t]est for (g)o";
      };
    }
    {
      key = "<leader>de";
      mode = "n";
      action = ":lua require'dap'.repl.open()<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "[d]ap r[e]pl open";
      };
    }
    {
      key = "<leader>lp";
      mode = "n";
      action = ":lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "[l]og DAP [p]oint message";
      };
    }
    {
      key = "<F5>";
      mode = "n";
      action = ":lua require'dap'.continue()<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "Continue DAP debug";
      };
    }
    {
      key = "<F10>";
      mode = "n";
      action = ":lua require'dap'.step_over()<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "Step over DAP debug";
      };
    }
    {
      key = "<F11>";
      mode = "n";
      action = ":lua require'dap'.step_into()<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "Step into DAP debug";
      };
    }
    {
      key = "<F12>";
      mode = "n";
      action = ":lua require'dap'.step_out()<CR>";
      options = {
        silent = true;
        noremap = true;
        desc = "Stepout of DAP debug";
      };
    }
  ];
}
