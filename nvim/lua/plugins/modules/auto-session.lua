local M = {
	"rmagatti/auto-session",
	cond = vim.g.vscode == nil,
	enabled = true,
	lazy = false,
}

function M.config()
	require("auto-session").setup({
		log_level = "error",
		auto_session_enable_last_session = false,
		auto_session_root_dir = vim.fn.stdpath("data") .. "/sessions/",
		auto_session_enabled = true,
		auto_save_enabled = true,
		auto_restore_enabled = true,
		bypass_session_save_file_types = { "neo-tree" },
	})
	vim.o.sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions"
end

return M
