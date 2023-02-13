-- For managing window splits and resizing

local M = {
    'mrjones2014/smart-splits.nvim',
    cond = vim.g.vscode == nil,
    enabled = true,
    event = "VeryLazy",
}

function M.config()
  require('smart-splits').setup({
      default_amount = 3,
      wrap_at_edge = false,
      move_cursor_same_row = true,
      resize_mode = {
          quit_key = '<ESC>',
          resize_keys = { 'h', 'j', 'k', 'l' },
          silent = false,
      },
      ignored_events = {
          'BufEnter',
          'WinEnter',
      },
  })
end

return M
