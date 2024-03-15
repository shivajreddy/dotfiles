return {
	"folke/lazy",
	keys = {
		{ "<leader>l", false },
		{ "<leader>L", false },
		--  Lazy
		vim.keymap.set("n", "<Leader>L", ":Lazy<CR>", { desc = "Lazy", silent = true }),
	},
}
