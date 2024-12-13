local style = require("utils.style")

return {
  "saghen/blink.cmp",
  opts = {
    menu = {
      border = style.border_chars_outer_thin,
    },
    keymap = {
      ["<CR>"] = { "accept", "fallback" },
      ["<C-k>"] = { "show", "show_documentation", "hide_documentation" },
      ["<C-e>"] = { "hide", "fallback" },

      ["<Tab>"] = { "snippet_forward", "select_next", "fallback" },
      ["<S-Tab>"] = { "snippet_backward", "select_prev", "fallback" },

      ["<Up>"] = { "select_prev", "fallback" },
      ["<Down>"] = { "select_next", "fallback" },

      ["<C-u>"] = { "scroll_documentation_up", "fallback" },
      ["<C-d>"] = { "scroll_documentation_down", "fallback" },
    },
    completion = {
      keyword = {
        range = "full",
      },
      trigger = {
        show_in_snippet = false,
      },
      list = {
        selection = "preselect",
      },
      ghost_text = {
        enabled = false,
      },
    },
  },
}
