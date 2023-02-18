local M = {
	"folke/which-key.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	event = "VeryLazy",
}

function M.config()
	vim.opt.timeoutlen = 300
	require("which-key").setup({
		defaults = {
			color_devicons = true,
		},
		show_help = false,
		window = {
			border = "none",
			margin = { 2, 50, 2, 50 },
			winblend = 4,
		},
	})
end

return M
