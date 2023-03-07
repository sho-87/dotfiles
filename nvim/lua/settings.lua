vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.signcolumn = "yes"
vim.opt.laststatus = 3

vim.opt.fillchars = "eob: "
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.textwidth = 120
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.wrap = false

vim.opt.foldenable = false
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.scrolloff = 8
vim.opt.sidescrolloff = 8
vim.opt.mousemoveevent = false -- screws with toggleterm input

vim.opt.undofile = true
vim.opt.undodir = vim.fn.stdpath("data") .. "/undodir"

vim.opt.verbose = 0
vim.opt.report = 99999
vim.opt.shortmess:append("astWAIcF")

-- Diagnostics
vim.diagnostic.config({
	signs = true,
	update_in_insert = false,
	underline = false,
	severity_sort = true,
	float = {
		focusable = true,
		border = "rounded",
		source = "always",
	},
})

-- Yank to system clipboard
vim.api.nvim_set_option("clipboard", "unnamed")

-- GUI
vim.opt.guifont = { "FiraCode_NF", "Source_Code_Pro", "Noto_Sans", "Sans_Serif", ":h11" }
