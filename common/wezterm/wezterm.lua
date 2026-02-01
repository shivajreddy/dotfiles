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
        bg_color = '#333333',
        fg_color = '#ffffff',
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
    active_titlebar_bg = "#000000",
    inactive_titlebar_bg = "#000000",
}


-- Finally, return the configuration to wezterm:
return config
