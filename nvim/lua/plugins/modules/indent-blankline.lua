local M = {
  'lukas-reineke/indent-blankline.nvim',
  cond = vim.g.vscode == nil,
  enabled = true,
  event = { "BufReadPost", "BufNewFile" }
}

function M.config()
  require('indent_blankline').setup {
    space_char_blankline = " ",
    show_current_context = true,
    show_current_context_start = true,
  }
end

return M
