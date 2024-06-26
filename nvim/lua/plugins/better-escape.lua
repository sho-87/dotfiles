local M = {
  "max397574/better-escape.nvim",
  event = "InsertEnter",
}

function M.config()
  require("better_escape").setup({
    mapping = { "kj", "jj" }, -- a table with mappings to use
    timeout = vim.o.timeoutlen, -- the time in which the keys must be hit in ms. Use option timeoutlen by default
    clear_empty_lines = true, -- clear line after escaping if there is only whitespace
    keys = function()
      return vim.api.nvim_win_get_cursor(0)[2] > 1 and "<esc>l" or "<esc>"
    end,
  })
end

return M
