-- Changed from dashboard-nvim because it was buggy and using the update plugin
-- from lazyvim, called snacks by folke

-- Resolve username cross-platform: try $USERNAME (Windows) then $USER (Unix)
local function get_user()
  local f = io.popen("whoami")
  local full = f:read("*l")
  f:close()
  return full:match("\\(.+)$") or full
end

local WORK_USERNAME = "shiva.reddy"
local username = get_user()
-- local username = os.getenv("USERNAME") or os.getenv("USER") or ""
local is_work = username == WORK_USERNAME

-- Personal header lines (6 lines)
local personal_lines_smpl = {
  [[███████╗    ███╗   ███╗    ██████╗     ██╗     ]],
  [[██╔════╝    ████╗ ████║    ██╔══██╗    ██║     ]],
  [[███████╗    ██╔████╔██║    ██████╔╝    ██║     ]],
  [[╚════██║    ██║╚██╔╝██║    ██╔═══╝     ██║     ]],
  [[███████║    ██║ ╚═╝ ██║    ██║         ███████╗]],
  [[╚══════╝    ╚═╝     ╚═╝    ╚═╝         ╚══════╝]],
}

local personal_lines_3 = {
  [[███████╗██╗  ██╗██╗██╗   ██╗ █████╗ ]],
  [[██╔════╝██║  ██║██║██║   ██║██╔══██╗]],
  [[███████╗███████║██║██║   ██║███████║]],
  [[╚════██║██╔══██║██║╚██╗ ██╔╝██╔══██║]],
  [[███████║██║  ██║██║ ╚████╔╝ ██║  ██║]],
  [[╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝  ╚═╝  ╚═╝]],
}

-- Work header lines (6 lines)
local work_lines = {
  [[░██     ░██ ░██    ░██               ░██           ]],
  [[░██     ░██ ░██    ░██                             ]],
  [[░██     ░██ ░██ ░████████  ░███████  ░██ ░████████ ]],
  [[░██     ░██ ░██    ░██    ░██    ░██ ░██░██    ░██ ]],
  [[░██     ░██ ░██    ░██    ░█████████ ░██░██    ░██ ]],
  [[ ░██   ░██  ░██    ░██    ░██        ░██░██    ░██ ]],
  [[  ░██████   ░██     ░████  ░███████  ░██ ░████████ ]],
  [[                                               ░██ ]],
  [[                                          ░███████  ]],
}

local work_lines_2 = {
  [[┌─┐┬ ┬┬┬  ┬┌─┐   ┬─┐┌─┐┌┬┐┌┬┐┬ ┬ ┌─┐ ┬ ┬┬ ┌┬┐┌─┐┬┌─┐   ┌─┐┌─┐┌┬┐]],
  [[└─┐├─┤│└┐┌┘├─┤   ├┬┘├┤  ││ ││└┬┘ │└┘ │ ││  │ ├┤ ││ ┬   │  │ ││││]],
  [[└─┘┴ ┴┴ └┘ ┴ ┴ o ┴└─└─┘─┴┘─┴┘ ┴  └── └─┘┴─┘┴ └─┘┴└─┘ o └─┘└─┘┴ ┴]],
}

local work_lines_3 = {
  [[██╗   ██╗██╗  ████████╗███████╗██╗ ██████╗ ]],
  [[██║   ██║██║  ╚══██╔══╝██╔════╝██║██╔════╝ ]],
  [[██║   ██║██║     ██║   █████╗  ██║██║  ███╗]],
  [[██║   ██║██║     ██║   ██╔══╝  ██║██║   ██║]],
  [[╚██████╔╝███████╗██║   ███████╗██║╚██████╔╝]],
  [[ ╚═════╝ ╚══════╝╚═╝   ╚══════╝╚═╝ ╚═════╝ ]],
}

local header_lines = is_work and work_lines or personal_lines_smpl

-- Build header sections: one entry per line with its highlight group
local header_sections = {}
for i, line in ipairs(header_lines) do
  table.insert(header_sections, {
    text = { { line, hl = "DashboardHeader" } },
    align = "center",
  })
end

return {
  "folke/snacks.nvim",
  lazy = false,
  init = function()
    vim.api.nvim_create_autocmd("ColorScheme", {
      pattern = "*",
      callback = function()
        vim.api.nvim_set_hl(0, "DashboardHeader", { fg = "#ff0000" })
      end,
    })
    vim.api.nvim_set_hl(0, "DashboardHeader", { fg = "#ff0000" })
  end,
  opts = {
    dashboard = {
      sections = vim.list_extend(header_sections, {
        { padding = 1 },
        -- stylua: ignore
        ---@type snacks.dashboard.Item[]
        { section = "keys", gap = 1, padding = 1 },
        { section = "startup" },
      }),
      preset = {
        -- stylua: ignore
        ---@type snacks.dashboard.Item[]
        keys = {
          { icon = " ", key = "f", desc = "Find File", action = ":lua Snacks.dashboard.pick('files')" },
          { icon = " ", key = "n", desc = "New File", action = ":ene | startinsert" },
          { icon = " ", key = "g", desc = "Find Text", action = ":lua Snacks.dashboard.pick('live_grep')" },
          { icon = " ", key = "r", desc = "Recent Files", action = ":lua Snacks.dashboard.pick('oldfiles')" },
          { icon = " ", key = "c", desc = "Config", action = ":lua Snacks.dashboard.pick('files', {cwd = vim.fn.stdpath('config')})" },
          { icon = " ", key = "w", desc = "WezTerm", action = ":lua Snacks.dashboard.pick('files', {cwd = vim.fs.joinpath(vim.fn.expand('~'), 'dotfiles/common/wezterm')})" },
          { icon = " ", key = "z", desc = "Glazer", action = ":lua Snacks.dashboard.pick('files', {cwd = vim.fs.joinpath(vim.fn.expand('~'), 'dotfiles/windows/glzr')})" },
          { icon = " ", key = "s", desc = "Restore Session", section = "session" },
          { icon = " ", key = "x", desc = "Lazy Extras", action = ":LazyExtras" },
          { icon = "󰒲 ", key = "l", desc = "Lazy", action = ":Lazy" },
          { icon = " ", key = "q", desc = "Quit", action = ":qa" },
        },
      },
    },
  },
}
