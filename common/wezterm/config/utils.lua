---@type wezterm
local wezterm = require("wezterm")
local M = {}

---merge two tables, t2 overwriting t1
---@param t1 table
---@param t2 table
---@return table
M.tableMerge = function(t1, t2)
	for k, v in pairs(t2) do
		if type(v) == "table" then
			if type(t1[k] or false) == "table" then
				M.tableMerge(t1[k] or {}, t2[k] or {})
			else
				t1[k] = v
			end
		else
			t1[k] = v
		end
	end
	return t1
end

---check if a table contains a value
---@param t table
---@param val any
---@return boolean
M.tableContains = function(t, val)
	for _, v in ipairs(t) do
		if v == val then
			return true
		end
	end
	return false
end

---check if we're on windows
---@return boolean
M.is_windows = function()
	return wezterm.target_triple:find("windows") ~= nil
end

---check if we're on macOS
---@return boolean
M.is_darwin = function()
	return wezterm.target_triple:find("darwin") ~= nil
end

---check if we're on arch
---@return boolean
M.is_arch = function()
	return wezterm.target_triple:find("darwin") ~= nil
end

---check if we're on Linux
---@return boolean
M.is_linux = function()
	return wezterm.target_triple:find("linux") ~= nil
end

-- Function to get current working directory
M.get_current_working_dir = function(tab)
	local current_dir = tab.active_pane.current_working_dir

	if current_dir == nil then
		return nil
	end

	-- Extract the path string from either a URL object or plain string
	local path
	if type(current_dir) == "table" and current_dir.file_path then
		path = current_dir.file_path
	elseif type(current_dir) == "string" then
		path = current_dir
	else
		-- Try tostring as last resort (URL objects support this)
		path = tostring(current_dir)
	end

	-- Strip file:// prefix if present
	path = path:gsub("^file://[^/]*", "")

	-- Strip trailing slashes
	path = path:gsub("[/\\]+$", "")

	-- Check if home directory
	local home = wezterm.home_dir:gsub("[/\\]+$", "")
	if path == home then
		return "~"
	end

	-- Return just the last folder name
	local basename = path:match("([^/\\]+)$")
	return basename or path
end

-- Function to retrieve the tab title
-- Priority: explicit tab title > CWD folder name > pane title (process name)
M.tab_title = function(tab_info)
	local title = tab_info.tab_title
	-- If the tab title is explicitly set, return that
	if title and #title > 0 then
		return title
	end
	-- Try the current working directory
	local cwd = M.get_current_working_dir(tab_info)
	if cwd then
		return cwd
	end
	-- Fall back to the active pane's title (process name), cleaned up
	local pane_title = tab_info.active_pane.title or ""
	-- Strip common Windows path prefixes to just get the process name
	pane_title = pane_title:gsub(".*[/\\]", "")
	-- Remove .exe suffix
	pane_title = pane_title:gsub("%.exe$", "")
	if #pane_title > 0 then
		return pane_title
	end
	return "shell"
end

return M
