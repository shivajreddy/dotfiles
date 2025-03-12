return {
  "nvim-neo-tree/neo-tree.nvim",
  enabled = false,
  opts = {
    window = {
      -- position = "right",
    },
    filesystem = {
      filtered_items = {
        hide_dotfiles = false, -- Hide dotfiles like .gitignore
        hide_gitignored = false, -- Hide files ignored by .gitignore
        hide_by_name = {
          "Cargo.lock", -- Add Cargo.lock to the list of hidden files
          "target",
          "venv",
          "_requirements.txt",
          "_watch.py",
        },
      },
    },
  },
}
