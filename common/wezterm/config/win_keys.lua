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
-- 'hjkl' to move between panes
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

map(
	"r",
	{ "LEADER", "ALT" },
	act.ActivateKeyTable({
		name = "resize_mode",
		one_shot = false,
	})
)

-- Workspaces Keybindings
-- Show Workspaces Launcher with ALT + SHIFT + G
map("g", { "ALT|SHIFT" }, wezterm.action.ShowLauncherArgs({ flags = "WORKSPACES" }))

-- New keybinding to change directory to dotfiles
-- Sends the string "cd /home/shiva/dotfiles" followed by Enter
-- local wsl_dotfiles = "\\wsl.localhost\Ubuntu\home\shiva\dotfiles\"
local wsl_dotfiles = "//wsl.localhost/Ubuntu/home/shiva/dotfiles/"

-- Switch to Workspace via Project Selection with ALT + G
map(
	"g",
	{ "ALT" },

	wezterm.action_callback(function(window, pane)
		local projects = {}
		local wezterm = require("wezterm") -- Ensure wezterm is required within the callback

		local wsl_path = "//wsl$/Ubuntu/home/shiva"
		local wsl_dotfiles = "//wsl$/Ubuntu/home/shiva/dotfiles"

		-- Define the directories to search for Git repositories
		local search_dirs = {
			-- Adjust the paths below based on your system and project locations
			"/dotfiles",
			wsl_path,
			wezterm.home_dir .. "\\home",
			-- Add more paths here
		}

		-- Run 'fd.exe' to find '.git' directories
		local success, stdout, stderr = wezterm.run_child_process({
			"fd.exe", -- Directly call 'fd.exe' assuming it's in PATH
			"-HI", -- Hidden files, ignore case
			"-td", -- Type: directory
			"^.git$", -- Regex to match '.git' directories
			"--max-depth=4", -- Limit search depth
		}, search_dirs) -- Pass search_dirs as arguments

		if not success then
			wezterm.log_error("Failed to run fd: " .. stderr)
			return
		end

		-- Add default project
		table.insert(projects, { label = wezterm.home_dir, id = "default" })

		-- Parse the output from 'fd.exe' and populate projects
		for line in stdout:gmatch("([^\n]*)\n?") do
			local project = line:gsub("\\.git$", "")
			local label = project
			local id = project:gsub(".*\\", "")
			table.insert(projects, { label = tostring(label), id = tostring(id) })
		end

		-- Display the project selector
		window:perform_action(
			wezterm.action.PromptInputLine({
				description = "Select Workspace",
				action = wezterm.action_callback(function(win, _, id, label)
					if not id and not label then
						wezterm.log_info("Cancelled")
					else
						wezterm.log_info("Selected " .. label)
						win:perform_action(
							wezterm.action.SwitchToWorkspace({ name = id, spawn = { cwd = label } }),
							pane
						)
					end
				end),
				-- You can customize the selector further if needed
			}),
			pane
		)
	end)
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
