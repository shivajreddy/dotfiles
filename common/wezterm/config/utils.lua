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

---check if we're on Linux
---@return boolean
M.is_linux = function()
	return wezterm.target_triple:find("linux") ~= nil
end

-- Function to get current working directory
M.get_current_working_dir = function(tab)
	local current_dir = tab.active_pane.current_working_dir
	if current_dir == nil then
		return "."
	end

	local HOME_DIR = string.format("file://%s", wezterm.home_dir)

	if type(current_dir) == "string" then
		return current_dir == HOME_DIR and "~" or string.gsub(current_dir, "(.*[/\\])(.*)", "%2")
	elseif type(current_dir) == "table" and current_dir.file_path then
		local path = current_dir.file_path
		return path == wezterm.home_dir and "~" or string.gsub(path, "(.*[/\\])(.*)", "%2")
	else
		return "."
	end
end

-- Function to retrieve the tab title or fall back to the active pane's working directory
M.tab_title = function(tab_info)
	local title = tab_info.tab_title
	-- If the tab title is explicitly set, return that
	if title and #title > 0 then
		return title
	end
	-- Otherwise, return the active pane's current working directory
	return "❯ " .. M.get_current_working_dir(tab_info)
end

return M
