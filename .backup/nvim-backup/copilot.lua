return {
  -- ── Copilot Chat ─────────────────────────────────────────────────────────
  -- Use this to talk to AI while coding: ask questions, fix code, explain stuff.
  -- Workflow: visual select the code you're stuck on → <leader>aq → describe what you need
  {
    "CopilotC-Nvim/CopilotChat.nvim",
    branch = "main",
    cmd = "CopilotChat",
    opts = function()
      local user = vim.env.USER or "User"
      user = user:sub(1, 1):upper() .. user:sub(2)
      return {
        auto_insert_mode = true,
        headers = {
          user = "  " .. user .. " ",
          assistant = "  Copilot ",
          tool = "󰊳  Tool ",
        },
        window = {
          width = 0.4,
        },
      }
    end,
    keys = {
      {
        "<c-s>",
        "<CR>",
        desc = "Submit Prompt",
        ft = "copilot-chat",
        remap = true,
      },
      {
        "<leader>a",
      },
      "",
      desc = "+ai",
      mode = { "n", "x" },
      {
        "<leader>aa",
        function()
          require("CopilotChat").toggle()
        end,
        desc = "Toggle Chat",
        mode = { "n", "x" },
      },
      {
        "<leader>ax",
        function()
          require("CopilotChat").reset()
        end,
        desc = "Clear Chat",
        mode = { "n", "x" },
      },
      {
        "<leader>ap",
        function()
          require("CopilotChat").select_prompt()
        end,
        desc = "Prompt Actions",
        mode = { "n", "x" },
      },
      {
        "<leader>aq",
        function()
          vim.ui.input({ prompt = "Ask Copilot: " }, function(input)
            if input ~= "" then
              require("CopilotChat").ask(input)
            end
          end)
        end,
        desc = "Quick Chat",
        mode = { "n", "x" },
      },
    },
    config = function(_, opts)
      local chat = require("CopilotChat")
      vim.api.nvim_create_autocmd("BufEnter", {
        pattern = "copilot-chat",
        callback = function()
          vim.opt_local.relativenumber = false
          vim.opt_local.number = false
        end,
      })
      chat.setup(opts)
    end,
  },

  -- ── blink.cmp: disable path source inside chat buffer ────────────────────
  {
    "saghen/blink.cmp",
    optional = true,
    opts = {
      sources = {
        providers = {
          path = {
            enabled = function()
              return vim.bo.filetype ~= "copilot-chat"
            end,
          },
        },
      },
    },
  },
}
