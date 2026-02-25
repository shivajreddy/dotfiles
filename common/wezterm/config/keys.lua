local wezterm = require("wezterm")
local act = wezterm.action
local utils = require("config.utils")
local session_manager = require("wezterm-session-manager/session-manager")

-- Session manager event bindings
wezterm.on("save_session", function(window)
	session_manager.save_state(window)
end)
wezterm.on("load_session", function(window)
	session_manager.load_state(window)
end)
wezterm.on("restore_session", function(window)
	session_manager.restore_state(window)
end)

local shortcuts = {}

local function map(key, mods, action)
	if type(mods) == "string" then
		table.insert(shortcuts, { key = key, mods = mods, action = action })
	elseif type(mods) == "table" then
		for _, mod in pairs(mods) do
			table.insert(shortcuts, { key = key, mods = mod, action = action })
		end
	end
end

-- Helper: Toggle tab bar
local toggleTabBar = wezterm.action_callback(function(window)
	window:set_config_overrides({
		enable_tab_bar = not window:effective_config().enable_tab_bar,
	})
end)

-- Helper: Open URL under cursor
local openUrl = act.QuickSelectArgs({
	label = "open url",
	patterns = { "https?://\\S+" },
	action = wezterm.action_callback(function(window, pane)
		local url = window:get_selection_text_for_pane(pane)
		wezterm.open_with(url)
	end),
})

-- Helper: Rename tab
local renameTab = act.PromptInputLine({
	description = "Enter new name for tab",
	action = wezterm.action_callback(function(window, _, line)
		if line then
			window:active_tab():set_title(line)
		end
	end),
})

-- Helper: Convert Unix path to Windows path
local function toWindowsPath(path)
	if path:sub(1, 1) == "/" then
		path = path:sub(2)
	end
	return path:gsub("/", "\\")
end

-- Helper: Get CWD from pane
local function getCwd(pane)
	local cwd = pane:get_current_working_dir()
	if cwd then
		return toWindowsPath(cwd.file_path or tostring(cwd))
	end
	return nil
end

-- Helper: Spawn tab in current directory
local spawnTabInCwd = wezterm.action_callback(function(window, pane)
	local cwd_path = getCwd(pane)
	window:perform_action(act.SpawnCommandInNewTab({ cwd = cwd_path }), pane)
end)

-- ===========================================
-- SPLITS (inherit CWD)
-- ===========================================
map(
	'"',
	"SHIFT|LEADER",
	wezterm.action_callback(function(window, pane)
		local cwd_path = getCwd(pane)
		window:perform_action(act.SplitHorizontal({ domain = "CurrentPaneDomain", cwd = cwd_path }), pane)
	end)
)

map(
	"'",
	"LEADER",
	wezterm.action_callback(function(window, pane)
		local cwd_path = getCwd(pane)
		window:perform_action(act.SplitVertical({ domain = "CurrentPaneDomain", cwd = cwd_path }), pane)
	end)
)

-- ===========================================
-- TAB NAVIGATION
-- ===========================================
map("Tab", "CTRL", act.ActivateTabRelative(1))
map("Tab", "SHIFT|CTRL", act.ActivateTabRelative(-1))

for i = 1, 9 do
	map(tostring(i), "CTRL", act.ActivateTab(i - 1))
end
map("0", "CTRL", act.ActivateTab(9))

-- ===========================================
-- PANE NAVIGATION
-- ===========================================
map("h", "LEADER", act.ActivatePaneDirection("Left"))
map("j", "LEADER", act.ActivatePaneDirection("Down"))
map("k", "LEADER", act.ActivatePaneDirection("Up"))
map("l", "LEADER", act.ActivatePaneDirection("Right"))

-- ===========================================
-- SPAWN & CLOSE
-- ===========================================
map("c", "LEADER", spawnTabInCwd)
map("x", "LEADER", act.CloseCurrentPane({ confirm = true }))
map("t", "SHIFT|CTRL", spawnTabInCwd)
map("n", "SHIFT|CTRL", act.SpawnWindow)

-- ===========================================
-- ZOOM & VIEW
-- ===========================================
map("z", { "LEADER", "CTRL" }, act.TogglePaneZoomState)
map("Z", "LEADER", toggleTabBar)

-- ===========================================
-- COPY & PASTE
-- ===========================================
map("v", "LEADER", act.ActivateCopyMode)
map("p", "LEADER", act.ActivateCopyMode)
map("p", "SHIFT|CTRL", act.ActivateCommandPalette)
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
	local tab, build_pane, window = wezterm.mux.spawn_window({
		workspace = "default",
		cwd = wezterm.home_dir,
	})
end)

map("s", "SHIFT|CTRL", workspace_switcher.switch_workspace())
map("y", "SHIFT|CTRL", act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" }))
map("u", "SHIFT|CTRL", act.SwitchWorkspaceRelative(1))
map("i", "SHIFT|CTRL", act.SwitchWorkspaceRelative(-1))

-- ===========================================
-- SESSION MANAGEMENT (save/load/restore)
-- ===========================================
map("S", "LEADER", act.EmitEvent("save_session"))
map("L", "LEADER", act.EmitEvent("load_session"))
map("R", "LEADER", act.EmitEvent("restore_session"))

-- ===========================================
-- WORKSPACE RENAME & DELETE
-- ===========================================
-- Rename current workspace
local renameWorkspace = act.PromptInputLine({
	description = "Enter new name for workspace",
	action = wezterm.action_callback(function(window, pane, line)
		if line then
			wezterm.mux.rename_workspace(wezterm.mux.get_active_workspace(), line)
		end
	end),
})
map("$", "LEADER", renameWorkspace) -- Shift+4 = $ (like tmux rename session)

-- Delete/close current workspace (closes all tabs/panes in workspace)
local deleteWorkspace = wezterm.action_callback(function(window, pane)
	local current_workspace = window:active_workspace()
	local workspaces = wezterm.mux.get_workspace_names()

	-- Don't delete if it's the only workspace
	if #workspaces <= 1 then
		wezterm.log_info("Cannot delete the only workspace")
		return
	end

	-- Switch to another workspace first
	for _, ws in ipairs(workspaces) do
		if ws ~= current_workspace then
			window:perform_action(act.SwitchToWorkspace({ name = ws }), pane)
			break
		end
	end

	-- Close all windows in the old workspace
	for _, mux_win in ipairs(wezterm.mux.all_windows()) do
		if mux_win:get_workspace() == current_workspace then
			mux_win:gui_window():perform_action(act.CloseCurrentTab({ confirm = false }), mux_win:active_pane())
		end
	end
end)
map("X", "LEADER", deleteWorkspace) -- Shift+x to delete workspace

-- ===========================================
-- SHIFT+ENTER (send CSI-u encoded sequence)
-- ===========================================
map("Enter", "SHIFT", act.SendString("\x1b[13;2u"))

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

function M.apply(c)
	-- Leader key
	if utils.is_windows() then
		c.leader = { key = "t", mods = "CTRL", timeout_milliseconds = math.maxinteger }
	elseif utils.is_darwin() then
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
end

return M
