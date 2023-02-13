local M = {
  'phaazon/hop.nvim',
  enabled = true,
  branch = 'v2',
  event = "VeryLazy",
}

function M.config()
  require('hop').setup {
    keys = 'etovxqpdygfblzhckisuran',
    multi_windows = true
  }
end

return M
