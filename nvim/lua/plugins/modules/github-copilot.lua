local M = {
	"zbirenbaum/copilot.lua",
	cond = vim.g.vscode == nil,
	enabled = true,
	cmd = "Copilot",
	dependencies = { "zbirenbaum/copilot-cmp" },
	event = "VeryLazy",
}

function M.config()
	require("copilot").setup({
		-- use cmp instead of panels and virtual text suggestions
		suggestion = { enabled = false },
		panel = { enabled = false },
	})
end

return M
