-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
local wk = require("which-key")

-- Delete some default keymaps
vim.api.nvim_del_keymap("n", "<leader>K")
vim.api.nvim_del_keymap("n", "<leader>L")
vim.api.nvim_del_keymap("n", "<leader>-")
vim.api.nvim_del_keymap("n", "<leader>|")
vim.api.nvim_del_keymap("n", "<C-w><C-D>")
vim.api.nvim_del_keymap("n", "<leader>xl")
vim.api.nvim_del_keymap("n", "<leader>xq")
vim.api.nvim_del_keymap("n", "<leader>l")
vim.api.nvim_del_keymap("n", "<leader>ft")
vim.api.nvim_del_keymap("n", "<leader>fT")
vim.api.nvim_del_keymap("n", "<leader>gG")
vim.api.nvim_del_keymap("n", "<leader>gf")

-- General
vim.keymap.set(
  "n",
  "<leader>qq",
  "<cmd>lua require('confirm-quit').confirm_quit_all({bang = false})<cr>",
  { desc = "Quit (confirm)" }
)
vim.keymap.set("n", "<leader>qQ", "<cmd>qa<cr>", { desc = "Quit" })
vim.keymap.set("v", "<C-c>", '"+y') -- Copy
vim.keymap.set("i", "<C-v>", '<esc>"+pi') -- Paste
vim.keymap.set("i", "<C-p>", "<C-r>0") -- Paste the last yank
vim.keymap.set("n", "<C-p>", '"0p') -- Paste the last yank
vim.keymap.set("i", "<C-H>", "<C-W>", { desc = "Delete word backward" }) -- Delete word backwards; some terminals: C-H = C-BS
vim.keymap.set("i", "<C-Del>", "<C-o>dw", { desc = "Delete word forward" }) -- Delete word forwards
vim.keymap.set({ "n", "x" }, "gg", "mggg", { desc = "Go to top of file" })
vim.keymap.set({ "n", "x" }, "G", "mgG", { desc = "Go to bottom of file" })
vim.keymap.set("n", "<leader>fs", "<cmd>w<cr><esc>", { desc = "Save file" })
vim.keymap.set("n", "<leader>uR", "<cmd>set cursorcolumn!<cr>", { desc = "Toggle column ruler" })

-- Windows
wk.add({ "<C-w>m", name = "Maximize", icon = "󰁌" })
vim.keymap.set("n", "<leader>wD", "<C-W>o", { desc = "Delete other windows" })
vim.keymap.set("n", "<leader>wo", "<C-W>p", { desc = "Other window" })
vim.keymap.set("n", "<leader><tab><tab>", function()
  vim.cmd("tabnew")
  require("telescope").extensions.project.project({})
end, { desc = "New tab" })

-- Tools
wk.add({ "<leader>z", group = "tools", icon = "" })
vim.keymap.set("n", "<leader>zl", "<cmd>Lazy<cr>", { desc = "Lazy" })
vim.keymap.set("n", "<leader>zm", "<cmd>Mason<cr>", { desc = "Mason" })
vim.keymap.set("n", "<leader>zh", "<cmd>LazyHealth<cr>", { desc = "Health" })
vim.keymap.set("n", "<leader>zs", "<cmd>StartupTime<cr>", { desc = "StartupTime" })
vim.keymap.set("n", "<leader>zL", "<cmd>LspInfo<cr>", { desc = "LspInfo" })
vim.keymap.set("n", "<leader>zi", "<cmd>Inspect<cr>", { desc = "Inspect" })

-- Terminal
wk.add({ "<leader>`", group = "terminal", icon = "" })
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], { buffer = 0 })
vim.keymap.set("t", "<C-h>", [[<Cmd>wincmd h<CR>]], { buffer = 0 })
vim.keymap.set("t", "<C-j>", [[<Cmd>wincmd j<CR>]], { buffer = 0 })
vim.keymap.set("t", "<C-k>", [[<Cmd>wincmd k<CR>]], { buffer = 0 })
vim.keymap.set("t", "<C-l>", [[<Cmd>wincmd l<CR>]], { buffer = 0 })
