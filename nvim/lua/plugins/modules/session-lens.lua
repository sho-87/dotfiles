local M = {
	"rmagatti/session-lens",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = { "rmagatti/auto-session", "nvim-telescope/telescope.nvim" },
	event = "VeryLazy",
}

function M.config()
	require("session-lens").setup({
		prompt_title = "Saved Sessions",
		path_display = { "shorten" },
		theme = "dropdown",
		theme_conf = { border = true },
		previewer = false,
	})
end

return M
