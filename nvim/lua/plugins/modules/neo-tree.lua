local M = {
	"nvim-neo-tree/neo-tree.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	branch = "v2.x",
	event = "VeryLazy",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-tree/nvim-web-devicons",
		"MunifTanjim/nui.nvim",
	},
}

function M.config()
	require("neo-tree").setup({
		close_if_last_window = true, -- Close Neo-tree if it is the last window left in the tab
		popup_border_style = "rounded",
		enable_git_status = true,
		enable_diagnostics = false,
		sort_case_insensitive = true, -- used when sorting files and directories in the tree
		window = {
			position = "left",
			width = 35,
		},
		source_selector = {
			winbar = false,
		},
		filesystem = {
			filtered_items = {
				visible = true, -- when true, they will just be displayed differently than normal items
				hide_dotfiles = false,
				hide_gitignored = false,
				hide_hidden = true, -- only works on Windows for hidden files/directories
			},
			follow_current_file = true,
			hide_by_name = {
				".DS_Store",
				"thumbs.db",
			},
			use_libuv_file_watcher = true, -- This will use the OS level file watchers to detect changes
		},
		git_status = {
			window = {
				position = "float",
			},
		},
	})
end

return M
