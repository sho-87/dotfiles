local M = {
  'nvim-lualine/lualine.nvim',
  cond = vim.g.vscode == nil,
  enabled = true,
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  event = { "BufReadPost", "BufNewFile" }
}

function M.config()
  require('lualine').setup {
    extensions = { 'neo-tree' }
  }
end

return M
