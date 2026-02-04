--[[
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::::::::::::::::::::     Author: Shiva        ::::::::::::::::::::
  ::::::::::::::::::::   Date: 11-20-2024       ::::::::::::::::::::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
--]]

-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices.

-- Use PowerShell as default shell on Windows
if wezterm.target_triple:find("windows") then
    config.default_prog = { 'pwsh.exe', '-NoLogo' }  --dont show ps version
    -- config.default_prog = { 'pwsh.exe' }
end

-- For example, changing the initial geometry for new windows:
config.initial_cols = 120
config.initial_rows = 28

-- or, changing the font size and color scheme.
config.font_size = 14
config.color_scheme = 'Catppuccin Mocha'

config.font = wezterm.font_with_fallback({
    { family = "Iosevka Nerd Font", weight = "Regular" },
    { family = "JetBrains Mono", weight = "Bold" },
    { family = "Noto Sans Mono", weight = "Bold" },
})


-- Background color                                                                                                      config.colors = {
config.colors = {
    -- background = '#0e0a01',
      background = '#000000',
    tab_bar = {
        background = '#111111',
        active_tab = {
        bg_color = '#DA3B01',
        fg_color = '#000000',
        },
        inactive_tab = {
        bg_color = '#1b1b1b',
        fg_color = '#888888',
        },
        inactive_tab_hover = {
        bg_color = '#222222',
        fg_color = '#ffffff',
        },
    }
}

-- Transparency (0.0 = fully transparent, 1.0 = opaque)
config.window_background_opacity = 0.90

-- On Windows, you can also use this for blur effect
-- config.win32_system_backdrop = 'Acrylic'  -- or 'Mica', 'Tabbed'

config.window_decorations = "RESIZE"

config.window_frame = {
    font = wezterm.font_with_fallback({
        { family = "Iosevka Nerd Font", weight = "Regular" },
        { family = "JetBrains Mono", weight = "Bold" },
        { family = "Noto Sans Mono", weight = "Bold" },
    }),
    font_size = 11.0,
    active_titlebar_bg = "#000000",
    inactive_titlebar_bg = "#000000",
}

-- Prevent tab title from changing when entering copy mode
config.use_fancy_tab_bar = false

-- Move tab bar to the bottom
config.tab_bar_at_bottom = true


-- Prevent pane title from showing "Copy mode" prefix and show tab numbers
wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    local title = tab.active_pane.title
    -- Remove "Copy mode: " prefix if present
    title = title:gsub("^Copy mode: ", "")
    -- Add tab number (1-indexed)
    local tab_number = tab.tab_index + 1
    
    -- Make active tab bold
    if tab.is_active then
        return {
            { Attribute = { Intensity = "Bold" } },
            { Text = " " .. tab_number .. " " .. title .. " " },
        }
    else
        return {
            { Text = " " .. tab_number .. " " .. title .. " " },
        }
    end
end)

-- Show LEADER and COPY MODE indicators in right status bar
wezterm.on("update-right-status", function(window, pane)
    local status = {}
    
    -- Set smaller font size for status bar
    table.insert(status, { Attribute = { Intensity = "Bold" } })
    
    -- Check for copy mode or other key tables
    local name = window:active_key_table()
    if name then
        local display_name = name:upper():gsub("_", " ")
        table.insert(status, { Foreground = { Color = "#000000" } })
        table.insert(status, { Background = { Color = "#a6e3a1" } })
        table.insert(status, { Text = " " .. display_name .. " " })
    end
    
    -- Check for leader mode
    if window:leader_is_active() then
        table.insert(status, { Foreground = { Color = "#000000" } })
        table.insert(status, { Background = { Color = "#f9e2af" } })
        table.insert(status, { Text = " LEADER " })
    end
    
    window:set_right_status(wezterm.format(status))
end)

-- Load key bindings
local win_keys = require("config.win_keys")
win_keys.apply(config)

-- Finally, return the configuration to wezterm:
return config
