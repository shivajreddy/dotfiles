---@type wezterm
local wezterm = require("wezterm")
local act = wezterm.action

local shortcuts = {}

---@param key string
---@param mods string|string[]
---@param action wezterm.Action
---@return nil
local map = function(key, mods, action)
	if type(mods) == "string" then
		table.insert(shortcuts, { key = key, mods = mods, action = action })
	elseif type(mods) == "table" then
		for _, mod in pairs(mods) do
			table.insert(shortcuts, { key = key, mods = mod, action = action })
		end
	end
end

local toggleTabBar = wezterm.action_callback(function(window)
	window:set_config_overrides({
		enable_tab_bar = not window:effective_config().enable_tab_bar,
	})
end)

local openUrl = act.QuickSelectArgs({
	label = "open url",
	patterns = { "https?://\\S+" },
	action = wezterm.action_callback(function(window, pane)
		local url = window:get_selection_text_for_pane(pane)
		wezterm.open_with(url)
	end),
})

--[[
-- NOTE

- Leader key is any thing that we set, see below to see what it is set to
map("t", { "SHIFT|CTRL", "ALT" }, act.SpawnTab("CurrentPaneDomain"))

this means that you can exucute this command in two ways:
1- shift+ctrl+t
2- alt+t


]]

-- use 'Backslash' to split horizontally
map("\\", "LEADER", act.SplitHorizontal({ domain = "CurrentPaneDomain" }))
-- and 'Minus' to split vertically
map("-", "LEADER", act.SplitVertical({ domain = "CurrentPaneDomain" }))
-- map 1-9 to switch to tab 1-9, 0 for the last tab
for i = 1, 9 do
	map(tostring(i), { "LEADER", "ALT" }, act.ActivateTab(i - 1))
end
map("0", { "LEADER", "ALT" }, act.ActivateTab(-1))
-- 'hjkl' to move between panes
-- map("h", { "LEADER", "ALT" }, act.ActivatePaneDirection("Left"))
-- map("j", { "LEADER", "ALT" }, act.ActivatePaneDirection("Down"))
-- map("k", { "LEADER", "ALT" }, act.ActivatePaneDirection("Up"))
-- map("l", { "LEADER", "ALT" }, act.ActivatePaneDirection("Right"))
map("h", { "LEADER", "ALT" }, act.ActivatePaneDirection("Left"))
map("j", { "LEADER", "ALT" }, act.ActivatePaneDirection("Down"))
map("k", { "LEADER", "ALT" }, act.ActivatePaneDirection("Up"))
map("l", { "LEADER", "ALT" }, act.ActivatePaneDirection("Right"))

-- resize
map("h", "LEADER|SHIFT", act.AdjustPaneSize({ "Left", 5 }))
map("j", "LEADER|SHIFT", act.AdjustPaneSize({ "Down", 5 }))
map("k", "LEADER|SHIFT", act.AdjustPaneSize({ "Up", 5 }))
map("l", "LEADER|SHIFT", act.AdjustPaneSize({ "Right", 5 }))
-- spawn & close
map("c", "LEADER", act.SpawnTab("CurrentPaneDomain"))
map("x", "LEADER", act.CloseCurrentPane({ confirm = true }))
map("t", { "SHIFT|CTRL", "ALT" }, act.SpawnTab("CurrentPaneDomain"))
map("w", { "SHIFT|CTRL", "ALT" }, act.CloseCurrentTab({ confirm = true }))
map("n", { "SHIFT|CTRL", "ALT" }, act.SpawnWindow)
-- zoom states
map("z", { "LEADER", "ALT" }, act.TogglePaneZoomState)
map("Z", { "LEADER", "ALT" }, toggleTabBar)
-- copy & paste
map("v", "LEADER", act.ActivateCopyMode)
map("c", { "SHIFT|CTRL", "ALT" }, act.CopyTo("Clipboard"))
map("v", { "SHIFT|CTRL", "ALT" }, act.PasteFrom("Clipboard"))
map("f", { "SHIFT|CTRL", "ALT" }, act.Search({ CaseInSensitiveString = "" }))
-- rotation
map("e", { "LEADER", "ALT" }, act.RotatePanes("Clockwise"))
-- pickers
map(" ", "LEADER", act.QuickSelect)
map("o", { "LEADER", "ALT" }, openUrl) -- https://github.com/shivajreddy
map("p", { "LEADER", "ALT" }, act.PaneSelect({ alphabet = "asdfghjkl;" }))
map("R", { "LEADER", "ALT" }, act.ReloadConfiguration)
map("u", "SHIFT|CTRL", act.CharSelect)
map("p", { "SHIFT|CTRL", "SHIFT|ALT" }, act.ActivateCommandPalette)
-- view
map("Enter", "ALT", act.ToggleFullScreen)
map("-", { "CTRL", "ALT" }, act.DecreaseFontSize)
map("=", { "CTRL", "ALT" }, act.IncreaseFontSize)
map("0", { "CTRL", "ALT" }, act.ResetFontSize)

-- debug
map("l", "SHIFT|CTRL", act.ShowDebugOverlay)

map(
	"r",
	{ "LEADER", "ALT" },
	act.ActivateKeyTable({
		name = "resize_mode",
		one_shot = false,
	})
)

local key_tables = {
	resize_mode = {
		{ key = "h", action = act.AdjustPaneSize({ "Left", 1 }) },
		{ key = "j", action = act.AdjustPaneSize({ "Down", 1 }) },
		{ key = "k", action = act.AdjustPaneSize({ "Up", 1 }) },
		{ key = "l", action = act.AdjustPaneSize({ "Right", 1 }) },
		{ key = "LeftArrow", action = act.AdjustPaneSize({ "Left", 1 }) },
		{ key = "DownArrow", action = act.AdjustPaneSize({ "Down", 1 }) },
		{ key = "UpArrow", action = act.AdjustPaneSize({ "Up", 1 }) },
		{ key = "RightArrow", action = act.AdjustPaneSize({ "Right", 1 }) },
	},
}

-- add a common escape sequence to all key tables
for k, _ in pairs(key_tables) do
	table.insert(key_tables[k], { key = "Escape", action = "PopKeyTable" })
	table.insert(key_tables[k], { key = "Enter", action = "PopKeyTable" })
	table.insert(key_tables[k], { key = "c", mods = "CTRL", action = "PopKeyTable" })
end

local M = {}
M.apply = function(c)
	c.leader = {
		key = "s",
		mods = "ALT",
		timeout_milliseconds = math.maxinteger,
	}
	c.debug_key_events = true
	c.keys = shortcuts
	c.disable_default_key_bindings = true
	c.key_tables = key_tables
end
return M
