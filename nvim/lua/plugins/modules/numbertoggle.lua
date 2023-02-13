-- switch between absolute and relative line numbers depending on mode

local M = {
  'sitiom/nvim-numbertoggle',
  cond = vim.g.vscode == nil,
  enabled = true,
  lazy = false,
}

function M.config()

end

return M
