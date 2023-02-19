-- for managing folds

local M = {
	"kevinhwang91/nvim-ufo",
	cond = vim.g.vscode == nil,
	enabled = false,
	dependencies = { "kevinhwang91/promise-async" },
	event = { "BufReadPost", "BufNewFile" },
}

function M.config()
	require("ufo").setup({
		provider_selector = function(bufnr, filetype, buftype)
			return { "treesitter", "indent" }
		end,
	})
	map_ufo()
end

return M
