local M = {
	"nvim-lualine/lualine.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = { "nvim-tree/nvim-web-devicons" },
	event = { "BufReadPost", "BufNewFile" },
}

function M.config()
	require("lualine").setup({
		options = {
			theme = "dracula",
		},
		sections = {
			lualine_a = { "mode" },
			lualine_b = { "branch", "diff", "diagnostics" },
			lualine_c = { "searchcount" },
			lualine_x = { "encoding", "fileformat", "filetype" },
			lualine_y = {},
			lualine_z = { "progress" },
		},
		inactive_sections = {
			lualine_a = { "" },
			lualine_b = { "" },
			lualine_c = {},
			lualine_x = { { "filetype", color = { fg = "grey" }, colored = false } },
			lualine_y = {},
			lualine_z = { "" },
		},
		extensions = { "neo-tree" },
	})
end

return M
