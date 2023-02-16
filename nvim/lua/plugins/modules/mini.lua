local M = {
  'echasnovski/mini.nvim',
  cond = vim.g.vscode == nil,
  enabled = true,
  version = false,
  event = "VeryLazy",
}

function M.config()
  require('mini.basics').setup {
    options = {
      extra_ui = true, -- Extra UI features ('winblend', 'cmdheight=0', ...)
      win_borders = 'single', -- Presets for window borders ('single', 'double', ...)
      move_with_alt = true, -- Move cursor in Insert, Command, and Terminal mode with <M-hjkl>
    },
  }
  require('mini.map').setup {
    integrations = {
      require('mini.map').gen_integration.builtin_search(),
      require('mini.map').gen_integration.gitsigns(),
      require('mini.map').gen_integration.diagnostic(),
    },
    symbols = {
      encode = require('mini.map').gen_encode_symbols.dot('4x2'),
    },
    window = {
      focusable = true,
      show_integration_count = false,
      width = 10,
      winblend = 50,
    },
  }
  require('mini.animate').setup {
    cursor = { enable = false, }
  }
  require('mini.move').setup()
  require('mini.cursorword').setup()
  require('mini.pairs').setup()
end

return M