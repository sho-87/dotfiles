local M = {
	"code-biscuits/nvim-biscuits",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = "nvim-treesitter/nvim-treesitter",
	event = { "BufReadPost", "BufNewFile" },
}

function M.config()
	require("nvim-biscuits").setup({
		show_on_start = true,
		cursor_line_only = true,
		prefix_string = " 🔝 ",
	})
end

return M
