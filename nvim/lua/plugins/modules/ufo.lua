-- for managing folds

local M = {
  'kevinhwang91/nvim-ufo',
  cond = vim.g.vscode == nil,
  enabled = true,
  dependencies = { 'kevinhwang91/promise-async' },
  event = "VeryLazy",
}

function M.config()
  require('ufo').setup {
    provider_selector = function(bufnr, filetype, buftype)
      return { 'treesitter', 'indent' }
    end
  }
end

return M