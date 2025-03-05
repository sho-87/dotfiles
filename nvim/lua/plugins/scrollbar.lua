return {
  {
    "lewis6991/satellite.nvim",
    opts = {
      current_only = true,
      width = 1,
      winblend = 0,
      excluded_filetypes = { "TelescopePrompt", "bigfile" },
      handlers = {
        cursor = {
          symbols = { "●" },
        },
      },
    },
  },
}
