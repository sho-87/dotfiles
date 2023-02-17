-- Open minimap on file open
vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufReadPost' }, {
  command = 'lua MiniMap.open()'
})

-- Offset bufferline when file tree is open
vim.api.nvim_create_autocmd('BufWinEnter', {
  callback = function(tbl)
    if vim.bo[tbl.buf].filetype == 'neo-tree' then
      require'bufferline.api'.set_offset(35, 'FileTree')
    end
  end
})

vim.api.nvim_create_autocmd({'BufWinLeave', 'BufWipeout'}, {
  callback = function(tbl)
    if vim.bo[tbl.buf].filetype == 'neo-tree' then
      require'bufferline.api'.set_offset(0)
    end
  end
})
