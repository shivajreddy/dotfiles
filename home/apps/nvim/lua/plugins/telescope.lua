local builtin = require("telescope.builtin")

return {
	"nvim-telescope/telescope.nvim",
	keys = {
		{ "<leader>/", false },
		-- Live Grep
		vim.keymap.set("n", "<Leader>l", builtin.live_grep, { desc = "Live Grep", silent = true }),
	},
}

--vim.keymap.set("n", "<Leader>l", "", { desc = "Live Grep", silent = true })
