local M = {
	"kylechui/nvim-surround",
	enabled = true,
	event = { "BufReadPost", "BufNewFile" },
}

function M.config()
	require("nvim-surround").setup({})
end

return M
