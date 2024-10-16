---@type wezterm
local wezterm = require("wezterm")
local act = wezterm.action

local utils = require("config.utils")

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

local renameTab = act.PromptInputLine({
	-- Function to handle renaming the tab
	description = "Enter new name for tab",
	action = wezterm.action_callback(function(window, _, line)
		if line then
			window:active_tab():set_title(line)
		end
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
	map(tostring(i), { "LEADER", "CMD" }, act.ActivateTab(i - 1))
end
map("0", { "LEADER", "CMD" }, act.ActivateTab(-1))
-- 'hjkl' to move between panes
map("h", { "LEADER", "CMD" }, act.ActivatePaneDirection("Left"))
map("j", { "LEADER", "CMD" }, act.ActivatePaneDirection("Down"))
map("k", { "LEADER", "CMD" }, act.ActivatePaneDirection("Up"))
map("l", { "LEADER", "CMD" }, act.ActivatePaneDirection("Right"))

-- resize
map("h", "LEADER|SHIFT", act.AdjustPaneSize({ "Left", 5 }))
map("j", "LEADER|SHIFT", act.AdjustPaneSize({ "Down", 5 }))
map("k", "LEADER|SHIFT", act.AdjustPaneSize({ "Up", 5 }))
map("l", "LEADER|SHIFT", act.AdjustPaneSize({ "Right", 5 }))
-- spawn & close
map("c", "LEADER", act.SpawnTab("CurrentPaneDomain"))
map("x", "LEADER", act.CloseCurrentPane({ confirm = true }))
map("t", { "SHIFT|CMD" }, act.SpawnTab("CurrentPaneDomain"))
map("w", { "SHIFT|CMD" }, act.CloseCurrentTab({ confirm = true }))
map("n", { "SHIFT|CMD" }, act.SpawnWindow)
-- zoom states
map("z", { "LEADER", "CMD" }, act.TogglePaneZoomState)
map("Z", { "LEADER", "CMD" }, toggleTabBar)
-- copy & paste
map("v", "LEADER", act.ActivateCopyMode)
map("c", { "CMD" }, act.CopyTo("Clipboard"))
map("v", { "CMD" }, act.PasteFrom("Clipboard"))

map("f", { "CMD" }, act.Search({ CaseInSensitiveString = "" }))
-- rotation
map("e", { "LEADER", "CMD" }, act.RotatePanes("Clockwise"))
-- pickers
map(" ", "LEADER", act.QuickSelect)
map("o", { "LEADER", "CMD" }, openUrl) -- https://github.com/shivajreddy
map("p", { "LEADER", "CMD" }, act.PaneSelect({ alphabet = "asdfghjkl;" }))
-- map("R", { "LEADER", "CMD" }, act.ReloadConfiguration)
map("u", "SHIFT|CTRL", act.CharSelect)
map("p", { "SHIFT|CTRL", "SHIFT|CMD" }, act.ActivateCommandPalette)
-- view
map("Enter", "ALT", act.ToggleFullScreen)
map("-", { "ALT" }, act.DecreaseFontSize)
map("=", { "ALT" }, act.IncreaseFontSize)
map("0", { "ALT" }, act.ResetFontSize)

-- rename tab
map("r", { "SHIFT|CMD" }, renameTab)

-- debug
map("l", "SHIFT|CMD", act.ShowDebugOverlay)

map(
	"r",
	{ "LEADER", "CMD" },
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

-- MAC spesific
-- in mac, wezterm.os_name is 'nil'
if utils.is_darwin() then
	map("s", { "CMD" }, act.SendKey({ key = "s", mods = "CTRL" }))
end

local M = {}
M.apply = function(c)
	if utils.is_windows() then
		c.leader = {
			key = "s",
			mods = "ALT",
			timeout_milliseconds = math.maxinteger,
		}
	end
	if utils.is_darwin() then
		c.leader = {
			key = "t",
			mods = "CMD",
			timeout_milliseconds = math.maxinteger,
		}
	end

	c.debug_key_events = true
	c.keys = shortcuts
	c.disable_default_key_bindings = true
	c.key_tables = key_tables
end
return M
