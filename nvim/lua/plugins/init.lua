return {
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "kanagawa-paper",
      icons = {
        ft = {
          quarto = "📓",
        },
        kinds = {
          otter = "🦦 ",
        },
      },
    },
  },
  -- submodules
  { import = "plugins.colourschemes" },
  { import = "plugins.languages" },
  -- disabled
  { "nvimdev/dashboard-nvim", enabled = false },
  { "lukas-reineke/indent-blankline.nvim", enabled = false },
  { "echasnovski/mini.pairs", enabled = false },
}
