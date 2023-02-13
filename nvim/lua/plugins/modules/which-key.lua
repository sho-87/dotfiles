local M = {
  "folke/which-key.nvim",
  cond = vim.g.vscode == nil,
  enabled = true,
  event = "VeryLazy"
}

function M.config()
  require("which-key").setup {
    defaults = {
      color_devicons = true,
    },
    window = {
      border = "single"
    },
  }
end

return M
