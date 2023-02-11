local map = vim.keymap.set
local noremap = {
    noremap = true,
    silent = true
}

-- General
vim.g.mapleader = " "
map('i', 'jj', '<ESC>', noremap) -- Exit insert mode
map('n', '<ESC>', ':nohlsearch<CR>', noremap) -- Clear highlights on ESC

-- Hop
map('n', '<leader>h', ':HopChar2<CR>', { desc = "Hop to 2 chars" })
map('n', '<leader>/', ':HopPattern<CR>', { desc = "Hop to pattern" })

-- Splits
if vim.g.vscode then
  -- Call VSCode window commands directly
  map('n', '<leader>wv', '<Cmd>call VSCodeNotify("workbench.action.splitEditorRight")<CR>', noremap)
  map('n', '<leader>ws', '<Cmd>call VSCodeNotify("workbench.action.splitEditorDown")<CR>', noremap)
  map('n', '<leader>wq', '<Cmd>call VSCodeNotify("workbench.action.closeActiveEditor")<CR>', noremap)

  map('n', '<leader>wh', '<Cmd>call VSCodeNotify("workbench.action.focusLeftGroup")<CR>', noremap)
  map('n', '<leader>wj', '<Cmd>call VSCodeNotify("workbench.action.focusBelowGroup")<CR>', noremap)
  map('n', '<leader>wk', '<Cmd>call VSCodeNotify("workbench.action.focusAboveGroup")<CR>', noremap)
  map('n', '<leader>wl', '<Cmd>call VSCodeNotify("workbench.action.focusRightGroup")<CR>', noremap)

  map('n', '<leader>wH', '<Cmd>call VSCodeNotify("workbench.action.moveEditorToLeftGroup")<CR>', noremap)
  map('n', '<leader>wJ', '<Cmd>call VSCodeNotify("workbench.action.moveEditorToBelowGroup")<CR>', noremap)
  map('n', '<leader>wK', '<Cmd>call VSCodeNotify("workbench.action.moveEditorToAboveGroup")<CR>', noremap)
  map('n', '<leader>wL', '<Cmd>call VSCodeNotify("workbench.action.moveEditorToRightGroup")<CR>', noremap)

  map('n', '<leader>wr', '<Cmd>call VSCodeNotify("workbench.action.increaseViewSize")<CR>', noremap)
  map('n', '<leader>wR', '<Cmd>call VSCodeNotify("workbench.action.decreaseViewSize")<CR>', noremap)
else
  map('n', '<leader>wv', '<C-W>v', { desc = "Split: vertical" })
  map('n', '<leader>ws', '<C-W>s', { desc = "Split: horizontal" })
  map('n', '<leader>wq', '<C-W>q', { desc = "Split: close" })

  map('n', '<leader>wh', require('smart-splits').move_cursor_left, { desc = "Focus: left" })
  map('n', '<leader>wj', require('smart-splits').move_cursor_down, { desc = "Focus: down" })
  map('n', '<leader>wk', require('smart-splits').move_cursor_up, { desc = "Focus: up" })
  map('n', '<leader>wl', require('smart-splits').move_cursor_right, { desc = "Focus: right" })

  map('n', '<leader>wH', '<C-W>H', { desc = "Move: left" })
  map('n', '<leader>wJ', '<C-W>J', { desc = "Move: down" })
  map('n', '<leader>wK', '<C-W>K', { desc = "Move: up" })
  map('n', '<leader>wL', '<C-W>L', { desc = "Move: right" })

  map('n', '<leader>wr', require('smart-splits').start_resize_mode, { desc = "Resize mode" })
end

-- File tree
if vim.g.vscode then
  map('n', '<leader>fb', '<Cmd>call VSCodeNotify("workbench.files.action.showActiveFileInExplorer")<CR>', noremap)
else
  map('n', '<leader>ff', require('telescope.builtin').find_files, { desc = "Find: files" })
  map('n', '<leader>fg', require('telescope.builtin').live_grep, { desc = "Find: grep" })
  map('n', '<leader>fb', require('telescope.builtin').buffers, { desc = "Find: buffers" })
  map('n', '<leader>fh', require('telescope.builtin').oldfiles, { desc = "Find: history" })
  map('n', '<leader>fs', require('telescope.builtin').grep_string, { desc = "Find: string" })
  map('n', '<leader>fm', require('telescope.builtin').keymaps, { desc = "Find: keymaps" })
  map('n', '<leader>fo', require('telescope.builtin').treesitter, { desc = "Find: outline" })
  map('n', '<leader>fb', ':Telescope file_browser<CR>', { desc = "Find: browse" })
end
