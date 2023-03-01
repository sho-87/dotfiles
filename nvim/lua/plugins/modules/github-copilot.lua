local M = {
	"zbirenbaum/copilot.lua",
	cond = vim.g.vscode == nil,
	enabled = true,
	cmd = "Copilot",
	event = "InsertEnter",
}

function M.config()
	require("copilot").setup({
		panel = {
			enabled = true,
			auto_refresh = false,
			keymap = {
				jump_prev = "[[",
				jump_next = "]]",
				accept = "<CR>",
				refresh = "gr",
				open = "<M-CR>",
			},
			layout = {
				position = "bottom", -- | top | left | right
				ratio = 0.4,
			},
		},
		suggestion = {
			enabled = true,
			auto_trigger = true,
			debounce = 75,
			keymap = {
				accept = "<M-l>",
				accept_word = "<C-l>",
				accept_line = false,
				next = "<C-]>",
				prev = "<C-[>",
				dismiss = "<C-e>",
			},
		},
	})
end

return M
