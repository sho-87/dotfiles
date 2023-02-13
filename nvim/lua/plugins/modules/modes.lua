-- change current line colour depending on mode

local M = {
  'mvllow/modes.nvim',
  cond = vim.g.vscode == nil,
  enabled = true,
  event = "VeryLazy"
}

function M.config()
  require('modes').setup()
end

return M
