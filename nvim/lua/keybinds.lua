-- General
vim.g.mapleader = " "
vim.api.nvim_set_keymap('i', 'jj', '<ESC>', {})

-- Clear highlights on ESC
vim.api.nvim_set_keymap('n', '<ESC>', ':nohlsearch<CR>', {
    noremap = true,
    silent = true
})

-- Hop
vim.api.nvim_set_keymap('n', '<Leader>h', ':HopChar2<CR>', {})
vim.api.nvim_set_keymap('n', '<Leader>/', ':HopPattern<CR>', {})

-- Splits
vim.api.nvim_set_keymap('n', '<Leader>wv', '<C-W>v', {}) -- Create split vertical
vim.api.nvim_set_keymap('n', '<Leader>ws', '<C-W>s', {}) -- Create split horizontal
vim.api.nvim_set_keymap('n', '<Leader>wq', '<C-W>q', {}) -- Close split

vim.api.nvim_set_keymap('n', '<Leader>wh', '<C-W>h', {}) -- Focus split
vim.api.nvim_set_keymap('n', '<Leader>wj', '<C-W>j', {})
vim.api.nvim_set_keymap('n', '<Leader>wk', '<C-W>k', {})
vim.api.nvim_set_keymap('n', '<Leader>wl', '<C-W>l', {})

vim.api.nvim_set_keymap('n', '<Leader>wH', '<C-W>H', {}) -- Move split
vim.api.nvim_set_keymap('n', '<Leader>wJ', '<C-W>J', {})
vim.api.nvim_set_keymap('n', '<Leader>wK', '<C-W>K', {})
vim.api.nvim_set_keymap('n', '<Leader>wL', '<C-W>L', {})
