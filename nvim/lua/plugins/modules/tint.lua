local M = {
	"levouh/tint.nvim",
	cond = vim.g.vscode == nil,
	enabled = false,
	lazy = false,
	priority = 0, -- make sure this is loaded last so highlight groups are not overridden
}

function M.config()
	require("tint").setup({
		-- tint = -50, -- Darken colors, use a positive value to brighten
		-- saturation = 0.2, -- Saturation to preserve
		transforms = {
			require("tint.transforms").tint_with_threshold(-100, "#191724", 250),
			require("tint.transforms").saturate(0.15),
		},
	})
end

return M
