local M = {
	"glepnir/dashboard-nvim",
	cond = vim.g.vscode == nil,
	enabled = false,
	dependencies = { "nvim-tree/nvim-web-devicons" },
	event = "VimEnter",
}

function M.config()
	require("dashboard").setup({
		theme = "hyper",
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
					icon_hl = "@variable",
					desc = " Files ",
					group = "Label",
					action = "Telescope find_files",
					key = "f",
				},
				{
					icon = "🔍",
					icon_hl = "@variable",
					desc = " Projects ",
					group = "Label",
					action = "Telescope project",
					key = "p",
				},
			},
			footer = { "", "-", "", "🐼 Never Half-Ass Two Things, Whole-Ass One Thing. 🐼" },
		},
	})
end

return M
