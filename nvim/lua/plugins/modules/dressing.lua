local M = {
	"stevearc/dressing.nvim",
	enabled = true,
    cond = vim.g.vscode == nil,
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
