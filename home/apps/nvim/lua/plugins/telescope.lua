return {
	"nvim-telescope/telescope.nvim",
	keys = {
    -- disable the leader+/ to grep files
		{ "<leader>/", false },
    -- Change a keymap
    { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find Files" },
		-- Live Grep
		-- vim.keymap.set("n", "<Leader>l", builtin.live_grep, { desc = "Live Grep", silent = true }),
    -- add a keymap to browse plugin files
    {
      "<leader>fp",
      function() requrie("telescope.builtin").find_files({ cwd = require("lua") })
      end,
      desc = "Find Plugin File"
    }
	},
}
