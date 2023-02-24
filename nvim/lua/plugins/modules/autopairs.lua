local M = {
	"windwp/nvim-autopairs",
	cond = vim.g.vscode == nil,
	enabled = true,
	event = { "InsertEnter" },
}

function M.config()
	require("nvim-autopairs").setup({
		check_ts = true,
		enable_check_bracket_line = false,
	})
end

return M
