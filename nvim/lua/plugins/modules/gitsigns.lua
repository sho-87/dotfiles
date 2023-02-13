local M = {
    'lewis6991/gitsigns.nvim',
    cond = vim.g.vscode == nil,
    enabled = true,
    event = "VeryLazy"
}

function M.config()
  require('gitsigns').setup()
end

return M
