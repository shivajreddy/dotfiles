return {
	"nvim-telescope/telescope.nvim",
	keys = {
		-- disable the keymap to grep files
		{ "<leader>/", false },
	},

	event = "ColorScheme",
	highlights = require("rose-pine.plugins.telescope"),
}
