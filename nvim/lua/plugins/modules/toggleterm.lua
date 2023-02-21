local M = {
	"akinsho/toggleterm.nvim",
	tag = "*",
	cond = vim.g.vscode == nil,
	enabled = true,
	cmd = { "ToggleTerm", "ToggleTermToggleAll", "TermExec" },
}

function M.config()
	require("toggleterm").setup({
		shell = "powershell.exe",
		start_in_insert = true,
		close_on_exit = true,
		shade_terminals = true,
		hide_numbers = true,
		autochdir = false,
		direction = "float",
	})
end

return M
