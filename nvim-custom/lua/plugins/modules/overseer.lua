local M = {
	"stevearc/overseer.nvim",
	enabled = true,
    cond = vim.g.vscode == nil,
	cmd = { "OverseerRun", "OverseerToggle" },
}

function M.config()
	require("overseer").setup({
		strategy = {
			"toggleterm",
			-- load your default shell before starting the task
			use_shell = true,
			-- overwrite the default toggleterm "direction" parameter
			direction = "horizontal",
			close_on_exit = false,
			-- open the toggleterm window when a task starts
			open_on_start = true,
			-- mirrors the toggleterm "hidden" parameter, and keeps the task from
			-- being rendered in the toggleable window
			hidden = false,
		},
		dap = true,
		auto_scroll = true,
		close_on_exit = false,
		open_on_start = true,
		templates = { "builtin", "scripts", "python", "terraform" },
	})
end

return M
