local map = vim.keymap.set
local vscode = vim.g.vscode
local noremap = {
    noremap = true,
    silent = true
}

-- General
vim.g.mapleader = " "
map('i', 'jj', '<ESC>', noremap) -- Exit insert mode
map('n', '<ESC>', ':nohlsearch<CR>', noremap) -- Clear highlights on ESC

-- Help
if vscode then
  map('n', '<leader>?k', '<Cmd>call VSCodeNotify("workbench.action.keybindingsReference")<CR>', noremap)
else
  map('n', '<leader>?', '{}', { desc = "Help" }) -- prefix
  map('n', '<leader>?k', require('telescope.builtin').keymaps, { desc = "Keymaps" })
end

-- Hop
map('n', '<leader>h', '{}', { desc = "Hop" }) -- prefix
map('n', '<leader>hh', ':HopChar2<CR>', { desc = "2 chars" })
map('n', '<leader>h/', ':HopPattern<CR>', { desc = "Pattern" })

-- Splits
if vscode then
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
  map('n', '<leader>w', '{}', { desc = "Window" }) -- prefix
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

-- Find
if vscode then
  map('n', '<leader>ff', '<Cmd>call VSCodeNotify("workbench.action.findInFiles")<CR>', noremap)
  map('n', '<leader>fr', '<Cmd>call VSCodeNotify("workbench.action.openRecent")<CR>', noremap)
  map('n', '<leader>fs', '<Cmd>call VSCodeNotify("editor.action.selectHighlights")<CR>', noremap)
  map('n', '<leader>fo', '<Cmd>call VSCodeNotify("outline.focus")<CR>', noremap)
  map('n', '<leader>fe', '<Cmd>call VSCodeNotify("workbench.files.action.showActiveFileInExplorer")<CR>', noremap)
  map('n', '<leader>fp', '<Cmd>call VSCodeNotify("projectManager.listProjects")<CR>', noremap)
else
  map('n', '<leader>f', '{}', { desc = "Find" }) -- prefix
  map('n', '<leader>ff', require('telescope.builtin').find_files, { desc = "Files" })
  map('n', '<leader>fg', require('telescope.builtin').live_grep, { desc = "Grep" })
  map('n', '<leader>fb', require('telescope.builtin').buffers, { desc = "Buffers" })
  map('n', '<leader>fr', require('telescope.builtin').oldfiles, { desc = "Recent" })
  map('n', '<leader>fs', require('telescope.builtin').grep_string, { desc = "String" })
  map('n', '<leader>fo', require('telescope.builtin').treesitter, { desc = "Outline" })
  map('n', '<leader>fe', require('telescope').extensions.file_browser.file_browser, { desc = "Explore" })
  map('n', '<leader>fp', require('telescope').extensions.project.project, { desc = "Project" })
end
