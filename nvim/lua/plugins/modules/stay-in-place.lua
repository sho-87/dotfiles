-- keep cursor in place when shifting (>>)
local M = {
	"gbprod/stay-in-place.nvim",
	enabled = false,
	event = { "BufReadPost", "BufNewFile" },
}

function M.config()
	require("stay-in-place").setup()
end

return M
