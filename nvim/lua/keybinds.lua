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
map('n', '<Leader>h', ':HopChar2<CR>', { desc = "Hop to 2 chars" })
map('n', '<Leader>/', ':HopPattern<CR>', { desc = "Hop to pattern" })

-- Splits
map('n', '<Leader>wv', '<C-W>v', { desc = "Split: vertical" })
map('n', '<Leader>ws', '<C-W>s', { desc = "Split: horizontal" })
map('n', '<Leader>wq', '<C-W>q', { desc = "Split: close" })

map('n', '<Leader>wh', '<C-W>h', { desc = "Focus: left" })
map('n', '<Leader>wj', '<C-W>j', { desc = "Focus: down" })
map('n', '<Leader>wk', '<C-W>k', { desc = "Focus: up" })
map('n', '<Leader>wl', '<C-W>l', { desc = "Focus: right" })

map('n', '<Leader>wH', '<C-W>H', { desc = "Move: left" })
map('n', '<Leader>wJ', '<C-W>J', { desc = "Move: down" })
map('n', '<Leader>wK', '<C-W>K', { desc = "Move: up" })
map('n', '<Leader>wL', '<C-W>L', { desc = "Move: right" })

map('n', '<Leader>w=', '<C-W>=', { desc = "Resize: equal" })
map('n', '<leader>w+', ':vertical resize +5<cr>', { desc = "Resize: Vertical +" }) -- increase VSplit
map('n', '<leader>w_', ':vertical resize -5<cr>', { desc = "Resize: Vertical -" })
