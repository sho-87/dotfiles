local M = {
    'glepnir/dashboard-nvim',
    cond = vim.g.vscode == nil,
    enabled = true,
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    event = 'VimEnter',
}

function M.config()
  require('dashboard').setup {
      theme = 'hyper',
      config = {
          week_header = { enable = true },
          packages = { enable = true },
          shortcut = {
              { desc = ' Update', group = '@property', action = 'Lazy update', key = 'u' },
              {
                  icon = ' ',
                  icon_hl = '@variable',
                  desc = 'Files',
                  group = 'Label',
                  action = 'Telescope find_files',
                  key = 'f',
              },
          },
          footer = { "", "", "🐼 Never Half-Ass Two Things, Whole-Ass One Thing. 🐼" },
      },
  }
end

return M
