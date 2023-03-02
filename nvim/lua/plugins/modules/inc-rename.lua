local M = {
	"smjonas/inc-rename.nvim",
    cond = vim.g.vscode == nil,
	enabled = true,
	event = { "BufRead", "BufNewFile" },
}

function M.config()
	require("inc_rename").setup()
end

return M
