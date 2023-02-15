local M = {
  'nvim-treesitter/nvim-treesitter',
  cond = vim.g.vscode == nil,
  enabled = true,
  event = "VeryLazy",
}

function M.config()
  require 'nvim-treesitter.install'.compilers = { "mingw", "clang", "gcc" }

  require 'nvim-treesitter.configs'.setup {
    ensure_installed = "all",
    auto_install = true, -- disable if no tree-sitter cli installed
    ignore_install = {}, -- list of parsers to ignore installing
    highlight = {
      enable = true,
      additional_vim_regex_highlighting = false
    },
    context_commentstring = {
      enable = true,
    },
  }
end

return M
