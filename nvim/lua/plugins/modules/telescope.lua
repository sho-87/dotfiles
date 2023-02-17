local M = {
  'nvim-telescope/telescope.nvim',
  cond = vim.g.vscode == nil,
  enabled = true,
  event = "VeryLazy",
  dependencies = { {
    'nvim-lua/plenary.nvim',
    'nvim-tree/nvim-web-devicons',
    { 'nvim-telescope/telescope-fzf-native.nvim' },
    'nvim-telescope/telescope-project.nvim',
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
      },
      fzf = {
        fuzzy = true, -- false will only do exact matching
        override_generic_sorter = true, -- override the generic sorter
        override_file_sorter = true, -- override the file sorter
        case_mode = "smart_case", -- or "ignore_case" or "respect_case"
      }
    }
  }
  require("telescope").load_extension("noice")
  require("telescope").load_extension("yank_history")
  require('telescope').load_extension('fzf')
end

return M
