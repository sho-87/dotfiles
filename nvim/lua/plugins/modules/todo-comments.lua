local M = {
	"folke/todo-comments.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = "nvim-lua/plenary.nvim",
	event = { "VeryLazy" },
}

function M.config()
	require("todo-comments").setup({})
end

return M
