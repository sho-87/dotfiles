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
    textobjects = {
      select = {
        enable = true,
        lookahead = true,
        keymaps = {
          -- You can use the capture groups defined in textobjects.scm
          ["af"] = { query = "@function.outer", desc = "around a function" },
          ["if"] = { query = "@function.inner", desc = "inner part of a function" },
          ["ac"] = { query = "@class.outer", desc = "around a class" },
          ["ic"] = { query = "@class.inner", desc = "inner part of a class" },
          ["ai"] = { query = "@conditional.outer", desc = "around an if statement" },
          ["ii"] = { query = "@conditional.inner", desc = "inner part of an if statement" },
          ["al"] = { query = "@loop.outer", desc = "around a loop" },
          ["il"] = { query = "@loop.inner", desc = "inner part of a loop" },
          ["ap"] = { query = "@parameter.outer", desc = "around parameter" },
          ["ip"] = { query = "@parameter.inner", desc = "inside a parameter" },
        },
        selection_modes = {
          ["@parameter.outer"] = "v", -- charwise
          ["@parameter.inner"] = "v", -- charwise
          ["@function.outer"] = "v", -- charwise
          ["@conditional.outer"] = "V", -- linewise
          ["@loop.outer"] = "V", -- linewise
          ["@class.outer"] = "<c-v>", -- blockwise
        },
        include_surrounding_whitespace = false,
      },
      move = {
        enable = true,
        set_jumps = true, -- whether to set jumps in the jumplist
        goto_previous_start = {
          ["[f"] = { query = "@function.outer", desc = "Previous function" },
          ["[c"] = { query = "@class.outer", desc = "Previous class" },
          ["[p"] = { query = "@parameter.inner", desc = "Previous parameter" },
        },
        goto_next_start = {
          ["]f"] = { query = "@function.outer", desc = "Next function" },
          ["]c"] = { query = "@class.outer", desc = "Next class" },
          ["]p"] = { query = "@parameter.inner", desc = "Next parameter" },
        },
      },
    },
  },
}

return M
