vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufReadPost' }, {
  command = 'lua MiniMap.open()'
})
