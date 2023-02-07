vim.g.mapleader = " "

-- Clear highlights on ESC
vim.api.nvim_set_keymap('n', '<ESC>', ':nohlsearch<CR>', {
    noremap = true,
    silent = true
})
