local M = {
	"zbirenbaum/copilot.lua",
	cond = vim.g.vscode == nil,
	enabled = true,
    dependencies = { "zbirenbaum/copilot-cmp" },
	cmd = "Copilot",
    event = "InsertEnter",
}

function M.config()
	require("copilot").setup({
		-- use cmp instead of panels and virtual text suggestions
		suggestion = { enabled = false },
		panel = { enabled = false },
	})
end

return M
