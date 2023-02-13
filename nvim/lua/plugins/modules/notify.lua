local M = {
    'rcarriga/nvim-notify',
    cond = vim.g.vscode == nil,
    enabled = true,
    event = 'VeryLazy',
}

function M.config()
  require("notify").setup {
      stages = 'slide',
      background_colour = 'FloatShadow',
  }
  vim.notify = require('notify')
end

return M
