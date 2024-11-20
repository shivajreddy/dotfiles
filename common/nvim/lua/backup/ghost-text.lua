return {
  "subnut/nvim-ghost.nvim",
  config = function()
    -- Set nvim-ghost to disable mode by default
    vim.g.nvim_ghost_disabled = 1

    -- Define autocmds for specific websites
    vim.cmd([[
      augroup nvim_ghost_user_autocommands
        autocmd!
        au User *coderbyte* setfiletype python
        au User *livecode.amazon* setfiletype python
      augroup END
    ]])
  end,

  enabled = false,
}
