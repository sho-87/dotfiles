local M = {
	"ggandor/leap.nvim",
	enabled = true,
	dependencies = "tpope/vim-repeat",
	event = "VeryLazy",
}

function M.config()
	require("leap").add_default_mappings()
end

M.opts = {
	highlight_unlabeled_phase_one_targets = true,
	max_highlighted_traversal_targets = 10,
	case_sensitive = true,
	safe_labels = { "s", "f", "n", "t" },
	labels = {
		"s",
		"f",
		"n",
		"j",
		"k",
		"l",
		"h",
		"d",
		"w",
		"e",
		"m",
		"b",
		"t",
		"F",
		"N",
		"J",
		"K",
		"L",
		"H",
		"D",
		"W",
		"E",
		"M",
		"B",
		"T",
	},
	special_keys = {
		repeat_search = "<enter>",
		next_phase_one_target = "<enter>",
		next_target = { "<enter>", ";" },
		prev_target = { "<tab>", "," },
		next_group = "<space>",
		prev_group = "<tab>",
		multi_accept = "<enter>",
		multi_revert = "<backspace>",
	},
}

return M
