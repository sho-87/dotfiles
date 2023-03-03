local M = {
	"stevearc/dressing.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	event = "VimEnter",
}

function M.config()
	require("dressing").setup({
		input = {
			enabled = false,
		},
	})
end

return M
