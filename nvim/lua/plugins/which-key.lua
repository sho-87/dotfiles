local utils = require("config.utils")

return {
  "folke/which-key.nvim",
  opts = {
    show_help = false,
    show_keys = false,
    preset = "helix",
    win = {
      title = true,
      border = utils.border_chars_outer_thin,
      padding = { 1, 0 },
    },
    layout = {
      align = "left",
    },
    icons = {
      group = "",
      keys = {
        C = "ᴄᴛʀʟ ",
        M = "ᴀʟᴛ ",
        D = "ᴄᴏᴍᴍᴀɴᴅ ",
        S = "󰘶 ",
      },
    },
    spec = {
      { "<leader>?", name = "Buffer keymaps" },
      { "<leader>q", name = "quit" },
      { "<leader>x", name = "diagnostics" },
      { "<leader>f", name = "find" },
    },
  },
}
