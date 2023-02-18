local M = {
	"levouh/tint.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	lazy = false,
	priority = 0,
}

function M.config()
	require("tint").setup({
		-- tint = -20, -- Darken colors, use a positive value to brighten
		-- saturation = 0, -- Saturation to preserve
		transforms = {
			require("tint.transforms").tint_with_threshold(-100, "#191724", 200), -- Try to tint by `-100`, but keep all colors at least `150` away from `#1C1C1C`
			require("tint.transforms").saturate(0),
		},
	})
end

return M
