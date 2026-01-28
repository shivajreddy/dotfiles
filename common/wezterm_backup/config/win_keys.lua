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
map("`", "LEADER", act.SplitVertical({ domain = "CurrentPaneDomain" }))

-- map 1-9 to switch to tab 1-9, 0 for the last tab
--[[
for i = 1, 9 do
	map(tostring(i), { "LEADER", "CTRL" }, act.ActivateTab(i - 1))
end
]]

-- Map CTRL + !@#$%^&*( to switch tabs 1-9, 0 for the last tab
local symbols = { "!", "@", "#", "$", "%", "^", "&", "*", "(", ")" }
for i, symbol in ipairs(symbols) do
	map(symbol, { "SHIFT|CTRL" }, act.ActivateTab(i - 1))
end

--[[
map("0", { "LEADER", "ALT" }, act.ActivateTab(-1))
map("h", { "LEADER", "ALT" }, act.ActivatePaneDirection("Left"))
map("j", { "LEADER", "ALT" }, act.ActivatePaneDirection("Down"))
map("k", { "LEADER", "ALT" }, act.ActivatePaneDirection("Up"))
map("l", { "LEADER", "ALT" }, act.ActivatePaneDirection("Right"))
]]
-- map("0", { "LEADER", "ALT" }, act.ActivateTab(-1))
map("h", { "LEADER", "SHIFT|CTRL" }, act.ActivatePaneDirection("Left"))
map("j", { "LEADER", "SHIFT|CTRL" }, act.ActivatePaneDirection("Down"))
map("k", { "LEADER", "SHIFT|CTRL" }, act.ActivatePaneDirection("Up"))
map("l", { "LEADER", "SHIFT|CTRL" }, act.ActivatePaneDirection("Right"))

-- spawn & close
map("c", "LEADER", act.SpawnTab("CurrentPaneDomain"))
map("x", "LEADER", act.CloseCurrentPane({ confirm = true }))
map("t", { "SHIFT|CTRL" }, act.SpawnTab("CurrentPaneDomain"))

-- map("w", { "SHIFT|CTRL" }, act.CloseCurrentTab({ confirm = true }))
map("n", { "SHIFT|CTRL" }, act.SpawnWindow)
-- zoom states
map("F", { "LEADER", "SHIFT|CTRL" }, act.TogglePaneZoomState)
map("Z", { "LEADER", "SHIFT|CTRL" }, toggleTabBar)
-- copy & paste
map("v", "LEADER", act.ActivateCopyMode)
map("c", { "SHIFT|CTRL" }, act.CopyTo("Clipboard"))

map("v", { "SHIFT|CTRL" }, act.PasteFrom("Clipboard"))
-- map("f", { "ALT" }, act.Search({ CaseInSensitiveString = "" }))

-- rotation
map("e", { "LEADER", "SHIFT|CTRL" }, act.RotatePanes("Clockwise"))
-- pickers
map(" ", "LEADER", act.QuickSelect)
map("o", { "LEADER", "SHIFT|CTRL" }, openUrl) -- https://github.com/shivajreddy
-- map("p", { "LEADER", "SHIFT|CTRL" }, act.PaneSelect({ alphabet = "asdfghjkl;" }))

map("p", { "SHIFT|CTRL", "SHIFT|CTRL" }, act.ActivateCommandPalette)

-- map("R", { "LEADER", "ALT" }, act.ReloadConfiguration)
--[[
map("u", "SHIFT|CTRL", act.CharSelect)
]]

-- view
--[[
map("Enter", "ALT", act.ToggleFullScreen)
map("-", { "ALT" }, act.DecreaseFontSize)
map("=", { "ALT" }, act.IncreaseFontSize)
map("0", { "ALT" }, act.ResetFontSize)
]]

-- rename tab
map("r", { "SHIFT|CTRL" }, renameTab)

-- debug
map("d", "SHIFT|CTRL", act.ShowDebugOverlay)

-- Reisze mode
map(
	"r",
	{ "LEADER" },
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

-- MAC specific
-- in mac, wezterm.os_name is 'nil'
if utils.is_darwin() then
	map("s", { "CMD" }, act.SendKey({ key = "s", mods = "CTRL" }))
end

-- TODO:
-- This finally fixed the workspaces
-- https://fredrikaverpil.github.io/blog/2024/10/20/session-management-in-wezterm-without-tmux/
-- load plugin
local workspace_switcher = wezterm.plugin.require("https://github.com/MLFlexer/smart_workspace_switcher.wezterm")
-- set path to zoxide
workspace_switcher.zoxide_path = "C:/Users/shiva/.cargo/bin/zoxide.exe"

wezterm.on("gui-startup", function(cmd)
	local dotfiles_path = wezterm.home_dir .. "/dotfiles"
	local tab, build_pane, window = wezterm.mux.spawn_window({
		workspace = "dotfiles",
		cwd = dotfiles_path,
		args = args,
	})
	-- build_pane:send_text("nvim\n")
	wezterm.mux.set_active_workspace("dotfiles")
end)

-- keymaps
map("s", { "SHIFT|CTRL" }, workspace_switcher.switch_workspace())
map("y", { "SHIFT|CTRL" }, act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" }))
map("u", { "SHIFT|CTRL" }, act.SwitchWorkspaceRelative(1))
map("i", { "SHIFT|CTRL" }, act.SwitchWorkspaceRelative(-1))

local M = {}
M.apply = function(c)
	if not c then
		error("Configuration 'c' is nil. Please pass a valid config.")
	end

	if utils.is_windows() then
		c.leader = {
			key = "T",
			mods = "CTRL",
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
