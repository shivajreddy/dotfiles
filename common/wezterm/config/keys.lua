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

-- ===========================================
-- OS-ADAPTIVE MODIFIER KEYS
-- On macOS: use CMD instead of CTRL so Karabiner swapping is not needed.
-- On Windows/Linux: keep CTRL as before.
-- ===========================================
local is_mac = utils.is_darwin()
-- Primary modifier (replaces bare CTRL bindings)
local C = is_mac and "CMD" or "CTRL"
-- Shifted primary modifier (replaces SHIFT|CTRL bindings)
local SC = is_mac and "SHIFT|CMD" or "SHIFT|CTRL"

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
		local path = cwd.file_path or tostring(cwd)
		if utils.is_windows() then
			return toWindowsPath(path)
		end
		return path
	end
	return nil
end

-- Helper: Spawn tab in home directory
local spawnTabInHome = wezterm.action_callback(function(window, pane)
	window:perform_action(act.SpawnCommandInNewTab({ cwd = wezterm.home_dir }), pane)
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
-- Cmd+Tab is owned by macOS (app switcher) and cannot be intercepted.
-- On macOS: Cmd+] / Cmd+[ for next/prev tab (common macOS convention).
-- On Windows/Linux: Ctrl+Tab / Ctrl+Shift+Tab as before.
-- Cmd+1-9 / Ctrl+1-9 for direct tab jumps (via C variable).
-- ===========================================
if is_mac then
	map("]", "SHIFT|CMD", act.ActivateTabRelative(1))
	map("[", "SHIFT|CMD", act.ActivateTabRelative(-1))
else
	map("Tab", "CTRL", act.ActivateTabRelative(1))
	map("Tab", "SHIFT|CTRL", act.ActivateTabRelative(-1))
end

for i = 1, 9 do
	map(tostring(i), C, act.ActivateTab(i - 1))
end
map("0", C, act.ActivateTab(9))

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
map("c", "LEADER", spawnTabInHome)
map("x", "LEADER", act.CloseCurrentPane({ confirm = true }))
map("t", SC, spawnTabInHome)
map("n", SC, act.SpawnWindow)

-- ===========================================
-- ZOOM & VIEW
-- ===========================================
map("z", { "LEADER", C }, act.TogglePaneZoomState)
map("z", SC, toggleTabBar)

-- ===========================================
-- COPY & PASTE
-- ===========================================
map("v", "LEADER", act.ActivateCopyMode)
map("p", "LEADER", act.ActivateCopyMode)
map("p", SC, act.ActivateCommandPalette)
map("b", SC, act.ScrollToBottom)
map("c", SC, act.CopyTo("Clipboard"))
map("v", SC, act.PasteFrom("Clipboard"))

-- ===========================================
-- PICKERS & MISC
-- ===========================================
map(" ", "LEADER", act.QuickSelect)
map("o", "LEADER", openUrl)
map("r", SC, renameTab)
map("e", "LEADER", act.RotatePanes("Clockwise"))
map("d", SC, act.ShowDebugOverlay)

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
if utils.is_windows() then
	workspace_switcher.zoxide_path = "C:/Users/shiva/.cargo/bin/zoxide.exe"
end
-- On macOS/Linux, zoxide is on PATH so no explicit path needed

wezterm.on("gui-startup", function(cmd)
	local tab, build_pane, window = wezterm.mux.spawn_window({
		workspace = "default",
		cwd = wezterm.home_dir,
	})
end)

map("s", SC, workspace_switcher.switch_workspace())
map("y", SC, act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" }))
map("u", SC, act.SwitchWorkspaceRelative(1))
map("i", SC, act.SwitchWorkspaceRelative(-1))

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
if is_mac then
	-- Restore expected macOS system bindings lost with disable_default_key_bindings
	-- Cmd+c sends Ctrl+c (interrupt/SIGINT) — use Cmd+y to copy instead
	-- map("c", "CMD", act.SendKey({ key = "c", mods = "CTRL" }))
	-- map("y", "CMD", act.CopyTo("Clipboard"))
	map("v", "CMD", act.PasteFrom("Clipboard"))
	map("q", "CMD", act.QuitApplication)
	map("m", "CMD", act.Hide)
	map("f", "CMD", act.ToggleFullScreen)
	-- Cmd+k is intentionally NOT mapped here — it's used by smart-splits
	-- for pane/nvim navigation (CMD+hjkl move focus). Use leader+k or
	-- the WezTerm debug overlay if you need to clear scrollback.
	-- map("k", "CMD", act.ClearScrollback("ScrollbackOnly"))

	-- Forward Cmd+key to nvim as Ctrl+key, so nvim <C-x> keymaps work
	-- without needing Karabiner to swap Cmd/Ctrl globally.
	-- Add any nvim Ctrl keymaps you use here.
	local nvim_ctrl_passthrough = {
		"c", -- <C-c> Cancel opeartion
		"e", -- <C-e> Scroll down
		"y", -- <C-y> Scroll up
		"s", -- <C-s> save
		"n", -- <C-n> neotree
		"d", -- <C-d> half-page down
		"u", -- <C-u> half-page up
		-- h/j/k/l are owned by smart-splits (CMD modifier in wezterm.lua)
		-- and should NOT also be forwarded here — smart-splits handles the
		-- nvim RPC call itself when in nvim, and activates pane when outside.
		"i", -- <C-i> hover docs
		"w", -- <C-w> delete word (shell readline)
		"/", -- <C-/> terminal toggle
	}
	for _, key in ipairs(nvim_ctrl_passthrough) do
		map(key, "CMD", act.SendKey({ key = key, mods = "CTRL" }))
	end
end

-- ===========================================
-- APPLY CONFIG
-- ===========================================
local M = {}

function M.apply(c)
	-- Leader key: CMD+t on macOS, CTRL+t elsewhere
	c.leader = {
		key = "t",
		mods = is_mac and "CMD" or "CTRL",
		timeout_milliseconds = math.maxinteger,
	}

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
