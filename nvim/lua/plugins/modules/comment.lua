local M = {
	"numToStr/Comment.nvim",
	enabled = true,
	event = { "BufReadPost", "BufNewFile" },
}

function M.config()
	require("Comment").setup()
end

return M
