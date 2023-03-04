local M = {
	"stevearc/overseer.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	cmd = { "OverseerRun", "OverseerToggle" },
}

function M.config()
	require("overseer").setup({
		strategy = "toggleterm",
		auto_scroll = nil,
		close_on_exit = false,
		open_on_start = true,
	})
end

return M
