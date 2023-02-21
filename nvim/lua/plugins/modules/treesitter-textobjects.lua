local M = {
	"nvim-treesitter/nvim-treesitter-textobjects",
	enabled = true,
	dependencies = "nvim-treesitter/nvim-treesitter",
	event = { "BufReadPost", "BufNewFile" },
}

return M
