local M = {
	"phaazon/hop.nvim",
	enabled = false,
	branch = "v2",
	cmd = { "HopChar1", "HopChar2", "HopPattern" },
}

function M.config()
	require("hop").setup({
		keys = "etovxqpdygfblzhckisuran",
		multi_windows = true,
	})
end

return M
