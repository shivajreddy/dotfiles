local wezterm = require("wezterm")
local config = wezterm.config_builder()

-- Set TERM_PROGRAM for smart-splits to detect wezterm
config.set_environment_variables = {
	TERM_PROGRAM = "WezTerm",
}

-- Use PowerShell on Windows
if wezterm.target_triple:find("windows") then
	config.default_prog = { "pwsh.exe", "-NoLogo" }
end

-- Load smart-splits plugin
-- On macOS: WezTerm listens on CMD+hjkl (so no Karabiner needed), but
-- sends CTRL+hjkl into nvim (so nvim's <C-h/j/k/l> bindings still fire).
-- On Windows/Linux: keep CTRL for both.
local smart_splits = wezterm.plugin.require("https://github.com/mrjones2014/smart-splits.nvim")
local is_mac = wezterm.target_triple:find("darwin") ~= nil
local ss_move_mod = is_mac and { wezterm = "CMD", neovim = "CTRL" } or "CTRL"
smart_splits.apply_to_config(config, {
	direction_keys = { "h", "j", "k", "l" },
	modifiers = {
		move = ss_move_mod,
		resize = "ALT",
	},
})

-- Load UI settings (colors, fonts, appearance)
require("config.ui").apply(config)

-- Load keybindings
require("config.keys").apply(config)

return config
