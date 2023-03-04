local M = {
	"stevearc/dressing.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	event = "VeryLazy",
}

function M.config()
	require("dressing").setup({
		input = {
			enabled = false,
		},
	})
end

return M
