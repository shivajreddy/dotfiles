---@type wezterm
local wezterm = require("wezterm")
local act = wezterm.action
local utils = require("config.utils")

local shortcuts = {}

---@param key string
---@param mods string|string[]
---@param action wezterm.Action
local map = function(key, mods, action)
	if type(mods) == "string" then
		table.insert(shortcuts, { key = key, mods = mods, action = action })
	elseif type(mods) == "table" then
		for _, mod in pairs(mods) do
			table.insert(shortcuts, { key = key, mods = mod, action = action })
		end
	end
end

-- Helper functions
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
	description = "Enter new name for tab",
	action = wezterm.action_callback(function(window, _, line)
		if line then
			window:active_tab():set_title(line)
		end
	end),
})

-- ===========================================
-- SPLITS
-- ===========================================
-- Note: smart-splits integration is loaded in wezterm.lua via plugin loader
-- ===========================================
map('"', "SHIFT|LEADER", act.SplitHorizontal({ domain = "CurrentPaneDomain" }))
map("'", "LEADER", act.SplitVertical({ domain = "CurrentPaneDomain" }))

-- ===========================================
-- TAB NAVIGATION
-- ===========================================
-- Ctrl+Tab / Ctrl+Shift+Tab to cycle through tabs
map("Tab", "CTRL", act.ActivateTabRelative(1))
map("Tab", "SHIFT|CTRL", act.ActivateTabRelative(-1))

-- Ctrl + 1-9 to switch to tabs 1-9, Ctrl+0 for tab 10
for i = 1, 9 do
	map(tostring(i), "CTRL", act.ActivateTab(i - 1))
end
map("0", "CTRL", act.ActivateTab(9))

-- ===========================================
-- PANE NAVIGATION
-- ===========================================
-- Note: Ctrl+h/j/k/l handled by smart-splits integration above
-- Leader+h/j/k/l kept as fallback
map("h", "LEADER", act.ActivatePaneDirection("Left"))
map("j", "LEADER", act.ActivatePaneDirection("Down"))
map("k", "LEADER", act.ActivatePaneDirection("Up"))
map("l", "LEADER", act.ActivatePaneDirection("Right"))

-- ===========================================
-- SPAWN & CLOSE
-- ===========================================
map("c", "LEADER", act.SpawnTab("CurrentPaneDomain"))
map("x", "LEADER", act.CloseCurrentPane({ confirm = true }))
map("t", "SHIFT|CTRL", act.SpawnTab("CurrentPaneDomain"))
map("n", "SHIFT|CTRL", act.SpawnWindow)

-- ===========================================
-- ZOOM & VIEW
-- ===========================================
map("z", "LEADER", act.TogglePaneZoomState)
map("Z", "LEADER", toggleTabBar)

-- ===========================================
-- COPY & PASTE
-- ===========================================
map("v", "LEADER", act.ActivateCopyMode)
map("p", "SHIFT|CTRL", act.ActivateCopyMode)
map("b", "SHIFT|CTRL", act.ScrollToBottom)
map("c", "SHIFT|CTRL", act.CopyTo("Clipboard"))
map("v", "SHIFT|CTRL", act.PasteFrom("Clipboard"))

-- ===========================================
-- PICKERS & MISC
-- ===========================================
map(" ", "LEADER", act.QuickSelect)
map("o", "LEADER", openUrl)
map("r", "SHIFT|CTRL", renameTab)
map("e", "LEADER", act.RotatePanes("Clockwise"))
map("d", "SHIFT|CTRL", act.ShowDebugOverlay)

-- ===========================================
-- RESIZE MODE
-- ===========================================
map("r", "LEADER", act.ActivateKeyTable({ name = "resize_mode", one_shot = false }))

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
		{ key = "Escape", action = "PopKeyTable" },
		{ key = "Enter", action = "PopKeyTable" },
		{ key = "c", mods = "CTRL", action = "PopKeyTable" },
	},
}

-- ===========================================
-- WORKSPACE SWITCHER
-- ===========================================
local workspace_switcher = wezterm.plugin.require("https://github.com/MLFlexer/smart_workspace_switcher.wezterm")
workspace_switcher.zoxide_path = "C:/Users/shiva/.cargo/bin/zoxide.exe"

wezterm.on("gui-startup", function(cmd)
	local dotfiles_path = wezterm.home_dir .. "/dotfiles"
	local tab, build_pane, window = wezterm.mux.spawn_window({
		workspace = "dotfiles",
		cwd = dotfiles_path,
	})
	wezterm.mux.set_active_workspace("dotfiles")
end)

map("s", "SHIFT|CTRL", workspace_switcher.switch_workspace())
map("y", "SHIFT|CTRL", act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" }))
map("u", "SHIFT|CTRL", act.SwitchWorkspaceRelative(1))
map("i", "SHIFT|CTRL", act.SwitchWorkspaceRelative(-1))

-- ===========================================
-- MAC SPECIFIC
-- ===========================================
if utils.is_darwin() then
	map("s", "CMD", act.SendKey({ key = "s", mods = "CTRL" }))
end

-- ===========================================
-- APPLY CONFIG
-- ===========================================
local M = {}

M.apply = function(c)
	if not c then
		error("Configuration 'c' is nil. Please pass a valid config.")
	end

	-- Leader key
	if utils.is_windows() then
		c.leader = { key = "t", mods = "CTRL", timeout_milliseconds = math.maxinteger }
	end
	if utils.is_darwin() then
		c.leader = { key = "t", mods = "CMD", timeout_milliseconds = math.maxinteger }
	end

	-- Merge with existing keys (e.g., from smart-splits plugin)
	if c.keys then
		for _, key in ipairs(shortcuts) do
			table.insert(c.keys, key)
		end
	else
		c.keys = shortcuts
	end
	
	c.disable_default_key_bindings = true
	c.key_tables = key_tables
	c.show_new_tab_button_in_tab_bar = false
end

return M
