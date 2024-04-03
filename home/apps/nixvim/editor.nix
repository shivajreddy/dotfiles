{
  pkgs,
  helpers,
  ...
}: {
  plugins = {
    neo-tree = {enable = true;};

    flash = {
      enable = true;
    };

    illuminate = {
      enable = true;
      delay = 200;
      largeFileOverrides.largeFileCutoff = 2000;
    };

    trouble = {
      enable = true;
      settings.use_diagnostic_signs = true;
      settings = {
        action_keys = {
        };
      };

      /*
      {
           keys = helpers.mkRaw ''
                 {
             {
             	"[q",
             	function()
             		if require("trouble").is_open() then
             			require("trouble").previous({ skip_groups = true, jump = true })
             		else
             			local ok, err = pcall(vim.cmd.cprev)
             			if not ok then
             				vim.notify(err, vim.log.levels.ERROR)
             			end
             		end
             	end,
             	desc = "Previous Trouble/Quickfix Item",
             },
             {
             	"]q",
             	function()
             		if require("trouble").is_open() then
             			require("trouble").next({ skip_groups = true, jump = true })
             		else
             			local ok, err = pcall(vim.cmd.cnext)
             			if not ok then
             				vim.notify(err, vim.log.levels.ERROR)
             			end
             		end
             	end,
             	desc = "Next Trouble/Quickfix Item",
             },
             }
           '';
      };
      */
    };

    todo-comments = {
      enable = true;
    };
  };
}
