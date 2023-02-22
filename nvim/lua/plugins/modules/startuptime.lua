local M = {
	"dstein64/vim-startuptime",
	cond = vim.g.vscode == nil,
	enabled = true,
	cmd = "StartupTime",
}

return M
