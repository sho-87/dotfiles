vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.signcolumn = "yes"

vim.opt.fillchars = "eob: "
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.wrap = false

vim.opt.foldenable = false
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = false
vim.opt.smartcase = true

vim.opt.scrolloff = 8
vim.opt.sidescrolloff = 8
vim.opt.winblend = 0
vim.opt.mousemoveevent = true

vim.opt.undofile = true
vim.opt.undodir = vim.fn.stdpath("data") .. "/undodir"

vim.opt.verbose = 0
vim.opt.report = 99999
vim.opt.shortmess:append("astWAIc")

-- Yank to system clipboard
vim.api.nvim_set_option("clipboard", "unnamed")

-- GUI
vim.opt.guifont = { "FiraCode_NF", "Source_Code_Pro", "Noto_Sans", "Sans_Serif", ":h11" }

if vim.g.neovide then
	vim.g.neovide_refresh_rate = 60
	vim.g.neovide_no_idle = true
end
