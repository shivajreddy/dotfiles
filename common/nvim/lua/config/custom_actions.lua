-- Personal custom actions — shown via '?' in normal mode
-- Each entry: { desc = "...", action = function() ... end, preview = { "line1", "line2", ... } }
-- `preview` is optional — if omitted, the desc is shown in the preview pane

return {
  {
    desc = "Hi",
    action = function()
      Snacks.notify("Hello, World!")
    end,
    preview = { "Sends a Hello World notification via Snacks.notify()" },
  },
  {
    desc = "Strip CRLF endings (remove ^M)",
    preview = {
      "Removes carriage return characters (\\r) from every line in the buffer.",
      "",
      "Useful when a file was saved on Windows (CRLF) but opened in Unix mode,",
      "causing ^M to appear at the end of each line.",
      "",
      "Runs: %s/\\r//ge",
    },
    action = function()
      vim.cmd([[%s/\r//ge]])
    end,
  },
}
