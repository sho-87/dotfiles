local M = {
	"nvim-telescope/telescope.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = {
		{
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons",
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
			"nvim-telescope/telescope-project.nvim",
			"debugloop/telescope-undo.nvim",
		},
	},
	cmd = "Telescope",
}

function M.config()
	require("telescope").setup({
		defaults = {
			layout_strategy = "horizontal",
			layout_config = { width = 0.8 },
			prompt_prefix = " 🔍 ",
		},
		extensions = {
			project = {
				base_dirs = {
					"~",
					"d:\\",
					"f:\\",
				},
				sync_with_nvim_tree = true,
			},
			undo = {
				use_delta = false,
				side_by_side = true,
				entry_format = "state #$ID, $STAT, $TIME",
				mappings = {
					n = {
						["<leader>ua"] = require("telescope-undo.actions").yank_additions,
						["<leader>ud"] = require("telescope-undo.actions").yank_deletions,
						["<leader>ur"] = require("telescope-undo.actions").restore,
					},
				},
			},
			fzf = {
				fuzzy = true, -- false will only do exact matching
				override_generic_sorter = true, -- override the generic sorter
				override_file_sorter = true, -- override the file sorter
				case_mode = "smart_case", -- or "ignore_case" or "respect_case"
			},
		},
	})
	require("telescope").load_extension("undo")
end

return M
