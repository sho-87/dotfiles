local M = {
  'numToStr/Comment.nvim',
  enabled = true,
  event = "VeryLazy",
}

function M.config()
  require('Comment').setup()
end

return M
