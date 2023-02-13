local M = {
    'nvim-treesitter/nvim-treesitter',
    cond = vim.g.vscode == nil,
    enabled = true,
    lazy = false,
    priority = 60,
}

function M.config()
  require 'nvim-treesitter.install'.compilers = { "mingw", "clang", "gcc" }

  require 'nvim-treesitter.configs'.setup {
      ensure_installed = { "c", "lua", "vim", "help", "cpp", "css", "comment", "dockerfile", "gitattributes",
          "gitcommit", "go", "hlsl", "html", "java", "javascript", "json", "json5", "julia",
          "kotlin", "latex", "markdown", "markdown_inline", "python", "r", "regex", "rust",
          "typescript", "yaml" },
      auto_install = true, -- disable if no tree-sitter cli installed
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
