--[[
return {
  "williamboman/mason.nvim",
  opts = function(_, opts)
    table.insert(opts.ensure_installed, "pyright")
    -- table.insert(opts.ensure_installed, "black")
    -- table.insert(opts.ensure_installed, "ruff")
    table.insert(opts.ensure_installed, "prettier")
  end,
}
--]]

-- temporary fix to install the specific version
--[[
return {
  { "mason-org/mason.nvim", version = "^1.0.0" },
  { "mason-org/mason-lspconfig.nvim", version = "^1.0.0" },
}
--]]

return {
  {
    "mason-org/mason.nvim",
    opts = {
      ensure_installed = {
        "clangd",
        "black",
      },
    },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = { "mason.nvim" },
    opts = {
      ensure_installed = {
        -- "clangd",
      },
    },
  },
}
