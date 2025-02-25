return {
  "nvimdev/dashboard-nvim",
  event = "VimEnter",
  opts = function()
    local _ = [[
                _____          __     __          ____________   _______    ______      _____         
          _____\    \        /  \   /  \        /            \  \      |  |      |   /      |_       
          /    / \    |      /   /| |\   \      |\___/\  \\___/|  |     /  /     /|  /         \      
        |    |  /___/|     /   //   \\   \      \|____\  \___|/  |\    \  \    |/  |     /\    \     
      ____\    \ |   ||    /    \_____/    \           |  |       \ \    \ |    |   |    |  |    \    
    /    /\    \|___|/   /    /\_____/\    \     __  /   / __     \|     \|    |   |     \/      \   
    |    |/ \    \       /    //\_____/\\    \   /  \/   /_/  |     |\         /|   |\      /\     \  
    |\____\ /____/|     /____/ |       | \____\ |____________/|     | \_______/ |   | \_____\ \_____\ 
    | |   ||    | |     |    | |       | |    | |           | /      \ |     | /    | |     | |     | 
    \|___||____|/      |____|/         \|____| |___________|/        \|_____|/      \|_____|\|_____| 
    ]]
    local _ = [[
━━           ┓   •              ━━━━
━━━━━━    ┏  ┣┓  ┓  ┓┏  ┏┓    ━━━━━━
━━        ┛  ┛┗  ┗  ┗┛  ┗┻      ━━━━
                                    
    ]]
    local logo = [[
━━━━━━    ┏┓  ┓   •            ━━━━━━
━━━━━━    ┗┓  ┣┓  ┓  ┓┏  ┏┓    ━━━━━━
━━━━━━    ┗┛  ┛┗  ┗  ┗┛  ┗┻    ━━━━━━
        ]]

    logo = string.rep("\n", 8) .. logo .. "\n\n"

    local opts = {
      theme = "doom",
      hide = {
        -- this is taken care of by lualine
        -- enabling this messes up the actual laststatus setting after loading a file
        statusline = true,
      },
      config = {
        header = vim.split(logo, "\n"),
          -- stylua: ignore
          center = {
            { action = "Telescope find_files", desc = " Find file", icon = " ", key = "f" },
            -- { action = "ene | startinsert", desc = " New file", icon = " ", key = "n" },
            { action = "Telescope oldfiles", desc = " Recent files", icon = " ", key = "r" },
            { action = "Telescope live_grep", desc = " Find text", icon = " ", key = "g" },

            { action = "Telescope find_files cwd=$HOME/obsidianvault/", desc = " Obsidian Notes", icon = " ", key = "n" },

            --stylua: ignore
            { action = "Telescope find_files cwd=~/dotfiles/home/apps/neovim/", desc = " Config", icon = " ", key = "c" },
            -- action = [[lua require("lazyvim.util").telescope.config_files()()]],

            { action = "Telescope find_files cwd=~/dotfiles/home/", desc = " Home", icon = " ", key = "h" },
            { action = 'lua require("persistence").load()', desc = " Restore Session", icon = " ", key = "s" },
            { action = "LazyExtras", desc = " Lazy Extras", icon = " ", key = "x" },
            { action = "Lazy", desc = " Lazy", icon = "󰒲 ", key = "l" },
            { action = "qa", desc = " Quit", icon = " ", key = "q" },
          },
        footer = function()
          local stats = require("lazy").stats()
          local ms = (math.floor(stats.startuptime * 100 + 0.5) / 100)
          -- return { "⚡ Neovim loaded " .. stats.loaded .. "/" .. stats.count .. " plugins in " .. ms .. "ms" }
          return { "" }
        end,
      },
    }

    for _, button in ipairs(opts.config.center) do
      button.desc = button.desc .. string.rep(" ", 43 - #button.desc)
      button.key_format = "  %s"
    end

    -- close Lazy and re-open when the dashboard is ready
    if vim.o.filetype == "lazy" then
      vim.cmd.close()
      vim.api.nvim_create_autocmd("User", {
        pattern = "DashboardLoaded",
        callback = function()
          require("lazy").show()
        end,
      })
    end

    return opts
  end,
}
