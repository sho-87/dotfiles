local M = {
    'nvim-telescope/telescope.nvim',
    cond = vim.g.vscode == nil,
    enabled = true,
    event = "VeryLazy",
    dependencies = { {
        -- consider telescope-fzf-native
        'nvim-lua/plenary.nvim',
        'nvim-tree/nvim-web-devicons',
        'nvim-telescope/telescope-project.nvim',
        'nvim-telescope/telescope-file-browser.nvim'
    } },
}

function M.config()
  require('telescope').setup {
      extensions = {
          project = {
              base_dirs = {
                  '~',
                  'd:\\',
                  'f:\\'
              },
              sync_with_nvim_tree = true,
          }
      }
  }
end

return M
