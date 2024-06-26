local M = {
  "nvim-treesitter/nvim-treesitter",
  opts = {
    ensure_installed = {
      "bash",
      "c",
      "comment",
      "cpp",
      "css",
      "dockerfile",
      "gitignore",
      "go",
      "gomod",
      "graphql",
      "html",
      "javascript",
      "jsdoc",
      "json",
      "json5",
      "jsonc",
      "lua",
      "markdown",
      "markdown_inline",
      "python",
      "query",
      "regex",
      "rst",
      "sql",
      "terraform",
      "toml",
      "tsx",
      "typescript",
      "vim",
      "vimdoc",
      "vue",
      "xml",
      "yaml",
    },
    auto_install = true, -- disable if no tree-sitter cli installed
    ignore_install = {}, -- list of parsers to ignore installing
    indent = { enable = true },
    highlight = {
      enable = true,
      additional_vim_regex_highlighting = false,
    },
    incremental_selection = {
      enable = false, -- use flash treesitter mode instead
    },
  },
}

return M
