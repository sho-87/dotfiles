local M = {
	"zbirenbaum/copilot-cmp",
	cond = vim.g.vscode == nil,
	enabled = true,
	event = "VeryLazy",
}

function M.config()
	require("copilot_cmp").setup()
end

return M
