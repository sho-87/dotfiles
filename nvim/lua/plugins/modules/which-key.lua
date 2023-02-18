local M = {
	"folke/which-key.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	event = "VeryLazy",
}

function M.config()
	vim.opt.timeoutlen = 500
	require("which-key").setup({
		defaults = {
			color_devicons = true,
		},
		window = {
			border = "none",
			margin = { 2, 2, 2, 2 },
		},
	})
end

return M
