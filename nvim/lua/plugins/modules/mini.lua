local M = {
	"echasnovski/mini.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	version = false,
	event = "VeryLazy",
}

function M.config()
	require("mini.basics").setup({
		options = {
			extra_ui = false, -- Extra UI features ('winblend', 'cmdheight=0', ...)
			win_borders = "single", -- Presets for window borders ('single', 'double', ...)
			move_with_alt = false, -- Move cursor in Insert, Command, and Terminal mode with <M-hjkl>
		},
		autocommands = {
			basic = false, -- yanky handles highlight on yank/put
		},
	})
	require("mini.move").setup()
end

return M
