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

-- Function to open a current selction in browser
local openUrl = act.QuickSelectArgs({
	label = "open url",
	patterns = { "https?://\\S+" },
	action = wezterm.action_callback(function(window, pane)
		local url = window:get_selection_text_for_pane(pane)
		wezterm.open_with(url)
	end),
})

-- Function to handle renaming the tab
local renameTab = act.PromptInputLine({
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
	map(tostring(i), { "LEADER", "ALT" }, act.ActivateTab(i - 1))
end
map("0", { "LEADER", "ALT" }, act.ActivateTab(-1))
map("h", { "LEADER", "ALT" }, act.ActivatePaneDirection("Left"))
map("j", { "LEADER", "ALT" }, act.ActivatePaneDirection("Down"))
map("k", { "LEADER", "ALT" }, act.ActivatePaneDirection("Up"))
map("l", { "LEADER", "ALT" }, act.ActivatePaneDirection("Right"))

-- spawn & close
map("c", "LEADER", act.SpawnTab("CurrentPaneDomain"))
map("x", "LEADER", act.CloseCurrentPane({ confirm = true }))
map("t", { "SHIFT|ALT" }, act.SpawnTab("CurrentPaneDomain"))

-- map("w", { "SHIFT|ALT" }, act.CloseCurrentTab({ confirm = true }))
map("n", { "SHIFT|ALT" }, act.SpawnWindow)
-- zoom states
map("z", { "LEADER", "ALT" }, act.TogglePaneZoomState)
map("Z", { "LEADER", "ALT" }, toggleTabBar)
-- copy & paste
map("v", "LEADER", act.ActivateCopyMode)
map("c", { "SHIFT|CTRL" }, act.CopyTo("Clipboard"))

map("v", { "SHIFT|CTRL" }, act.PasteFrom("Clipboard"))
map("f", { "ALT" }, act.Search({ CaseInSensitiveString = "" }))
-- rotation
map("e", { "LEADER", "ALT" }, act.RotatePanes("Clockwise"))
-- pickers
map(" ", "LEADER", act.QuickSelect)
map("o", { "LEADER", "ALT" }, openUrl) -- https://github.com/shivajreddy
map("p", { "LEADER", "ALT" }, act.PaneSelect({ alphabet = "asdfghjkl;" }))
-- map("R", { "LEADER", "ALT" }, act.ReloadConfiguration)
map("u", "SHIFT|CTRL", act.CharSelect)
map("p", { "SHIFT|CTRL", "SHIFT|ALT" }, act.ActivateCommandPalette)
-- view
map("Enter", "ALT", act.ToggleFullScreen)
map("-", { "ALT" }, act.DecreaseFontSize)
map("=", { "ALT" }, act.IncreaseFontSize)
map("0", { "ALT" }, act.ResetFontSize)

-- rename tab
map("r", { "SHIFT|ALT" }, renameTab)

-- debug
map("l", "SHIFT|ALT", act.ShowDebugOverlay)

-- Reisze mode
map(
	"r",
	{ "ALT" },
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

-- Smart Workspace switcher
local workspace_switcher = wezterm.plugin.require("https://github.com/MLFlexer/smart_workspace_switcher.wezterm")
map("s", { "ALT" }, workspace_switcher.switch_workspace())
-- Resurrect
local resurrect = wezterm.plugin.require("https://github.com/MLFlexer/resurrect.wezterm")
map(
	"w",
	"ALT",
	wezterm.action_callback(function(win, pane)
		resurrect.save_state(resurrect.workspace_state.get_workspace_state())
	end)
)
map("W", "ALT", resurrect.window_state.save_window_action())
-- map("T", "ALT", resurrect.tab_state.save_tab_action())
map(
	"S",
	"ALT",
	wezterm.action_callback(function(win, pane)
		resurrect.save_state(resurrect.workspace_state.get_workspace_state())
		resurrect.window_state.save_window_action()
	end)
)
map(
	"q",
	"ALT",
	wezterm.action_callback(function(win, pane)
		resurrect.fuzzy_load(win, pane, function(id, label)
			local type = string.match(id, "^([^/]+)") -- match before '/'
			id = string.match(id, "([^/]+)$") -- match after '/'
			id = string.match(id, "(.+)%..+$") -- remove file extension
			local opts = {
				relative = true,
				restore_text = true,
				on_pane_restore = resurrect.tab_state.default_on_pane_restore,
			}
			if type == "workspace" then
				local state = resurrect.load_state(id, "workspace")
				resurrect.workspace_state.restore_workspace(state, opts)
			elseif type == "window" then
				local state = resurrect.load_state(id, "window")
				resurrect.window_state.restore_window(pane:window(), state, opts)
			elseif type == "tab" then
				local state = resurrect.load_state(id, "tab")
				resurrect.tab_state.restore_tab(pane:tab(), state, opts)
			end
		end)
	end)
)

-- MAC specific
-- in mac, wezterm.os_name is 'nil'
if utils.is_darwin() then
	map("s", { "CMD" }, act.SendKey({ key = "s", mods = "CTRL" }))
end

local M = {}
M.apply = function(c)
	if not c then
		error("Configuration 'c' is nil. Please pass a valid config.")
	end

	if utils.is_windows() then
		c.leader = {
			key = "t",
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
