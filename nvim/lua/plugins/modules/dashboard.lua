local M = {
	"glepnir/dashboard-nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = { "nvim-tree/nvim-web-devicons" },
	event = "VimEnter",
}

function M.config()
	require("dashboard").setup({
		theme = "hyper",
		disable_move = true,
		shortcut_type = "number",
		config = {
			week_header = { enable = true },
			packages = { enable = false },
			shortcut = {
				{
					icon = "💤",
					desc = " Lazy ",
					group = "@property",
					action = "Lazy home",
					key = "l",
				},
				{
					icon = "🧱",
					desc = " Mason ",
					group = "@property",
					action = "Mason",
					key = "m",
				},
				{
					icon = "🔍",
					desc = " Files ",
					group = "@parameter",
					action = "Telescope find_files",
					key = "f",
				},
				{
					icon = "🔍",
					desc = " Projects ",
					group = "@parameter",
					action = "require'telescope'.extensions.project.project{}",
					key = "p",
				},
			},
			project = { enable = false },
			mru = { limit = 9 },
			footer = { "", "-", "", "🐼 Never Half-Ass Two Things, Whole-Ass One Thing. 🐼" },
		},
	})
end

return M
