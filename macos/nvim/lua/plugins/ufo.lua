-- Foldings plugin. https://github.com/kevinhwang91/nvim-ufo
return {
  "kevinhwang91/nvim-ufo",
  dependencies = {
    "kevinhwang91/promise-async",
  },
}
--[[
return {
  "kevinhwang91/nvim-ufo",
  dependencies = {
    "kevinhwang91/promise-async",
  },
  event = "BufReadPost",
  config = function()
    -- Custom fold provider for C++ files with // { and // } markers
    local function cpp_custom_provider(bufnr)
      local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
      local folds = {}
      local fold_stack = {}

      for i, line in ipairs(lines) do
        local line_num = i - 1 -- Convert to 0-based indexing

        -- Check for fold start marker
        if line:match("^%s*//.-{") then
          table.insert(fold_stack, line_num)
        end

        -- Check for fold end marker
        if line:match("^%s*//.-}") then
          if #fold_stack > 0 then
            local start_line = table.remove(fold_stack)
            table.insert(folds, {
              startLine = start_line,
              endLine = line_num,
              kind = "region",
            })
          end
        end
      end

      return folds
    end

    local ftMap = {
      vim = "indent",
      python = { "indent" },
      git = "",
      cpp = { "lsp", cpp_custom_provider },
      c = { "lsp", cpp_custom_provider }, -- Also apply to C files if needed
    }

    require("ufo").setup({
      open_fold_hl_timeout = 150,
      close_fold_kinds_for_ft = {
        default = { "imports", "comment" },
        json = { "array" },
        c = { "comment", "region" },
        cpp = { "comment", "region" },
      },
      preview = {
        win_config = {
          border = { "", "─", "", "", "", "─", "", "" },
          winhighlight = "Normal:Folded",
          winblend = 0,
        },
        mappings = {
          scrollU = "<C-u>",
          scrollD = "<C-d>",
          jumpTop = "[",
          jumpBot = "]",
        },
      },
      provider_selector = function(bufnr, filetype, buftype)
        return ftMap[filetype]
      end,
      fold_virt_text_handler = nil,
    })

    -- Keymaps
    vim.keymap.set("n", "zR", require("ufo").openAllFolds)
    vim.keymap.set("n", "zM", require("ufo").closeAllFolds)
    vim.keymap.set("n", "zr", require("ufo").openFoldsExceptKinds)
    vim.keymap.set("n", "zm", require("ufo").closeFoldsWith)

    -- Custom paragraph jump that respects folds
    vim.keymap.set("n", "{", function()
      -- Save current options
      local foldopen = vim.opt.foldopen:get()
      -- Temporarily disable opening folds when moving
      vim.opt.foldopen:remove("block")
      -- Move to previous paragraph
      vim.cmd("normal! {")
      -- Restore options
      vim.opt.foldopen = foldopen
    end, { noremap = true, silent = true })

    vim.keymap.set("n", "}", function()
      -- Save current options
      local foldopen = vim.opt.foldopen:get()
      -- Temporarily disable opening folds when moving
      vim.opt.foldopen:remove("block")
      -- Move to next paragraph
      vim.cmd("normal! }")
      -- Restore options
      vim.opt.foldopen = foldopen
    end, { noremap = true, silent = true })

    -- Peek fold with K
    vim.keymap.set("n", "K", function()
      local winid = require("ufo").peekFoldedLinesUnderCursor()
      if not winid then
        -- choose one of coc.nvim and nvim lsp
        vim.fn.CocActionAsync("definitionHover") -- coc.nvim
        vim.lsp.buf.hover()
      end
    end)
  end,
}
--]]
