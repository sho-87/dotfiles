return {
  {
    "shellRaining/hlchunk.nvim",
    event = "LazyFile",
    opts = {
      chunk = {
        enable = true,
        notify = true,
        use_treesitter = true,
        chars = {
          horizontal_line = "─",
          vertical_line = "│",
          left_top = "╭",
          left_bottom = "╰",
          right_arrow = "─",
        },
        style = {
          { fg = "#a292a3" },
          { fg = "#c4746e" }, -- this fg is used to highlight wrong chunk
        },
        textobject = "",
        max_file_size = 1024 * 1024,
        error_sign = true,
        exclude_filetypes = {
          ["neotest-summary"] = true,
          ["neo-tree-popup"] = true,
          ["ccc-ui"] = true,
          ["octo-panel"] = true,
          lazyterm = true,
        },
      },
      indent = {
        enable = false,
        use_treesitter = true,
        chars = {
          "│",
        },
        style = {
          { fg = "#363646" },
        },
      },
      line_num = {
        enable = false,
      },
      blank = {
        enable = false,
        chars = { " " },
        style = {
          { bg = "#2A2A37" },
          { bg = "", fg = "" },
        },
      },
    },
  },
}
