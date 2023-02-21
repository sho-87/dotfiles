local M = {
	"ggandor/flit.nvim",
	enabled = true,
	dependencies = "ggandor/leap.nvim",
	keys = { "f", "F", "t", "T" },
}

function M.config()
	require("flit").setup({
		keys = { f = "f", F = "F", t = "t", T = "T" },
		-- A string like "nv", "nvo", "o", etc.
		labeled_modes = "nv",
		multiline = true,
		opts = {},
	})
end

return M
