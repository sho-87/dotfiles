local M = {
	"romgrk/barbar.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = "nvim-tree/nvim-web-devicons",
	event = "VimEnter",
}

function M.config()
	require("bufferline").setup({
		animation = true,
		auto_hide = false,
		tabpages = true,
		closable = true,
		clickable = true,

		-- Hide inactive buffers and file extensions. Other options are `alternate`, `current`, and `visible`.
		hide = { extensions = false, inactive = false },

		-- Disable highlighting alternate buffers
		highlight_alternate = false,

		-- Disable highlighting file icons in inactive buffers
		highlight_inactive_file_icons = false,

		-- Enable highlighting visible buffers
		highlight_visible = true,

		icons = true,
		icon_pinned = "車",

		diagnostics = {
			[vim.diagnostic.severity.ERROR] = { enabled = true, icon = "ﬀ" },
			[vim.diagnostic.severity.WARN] = { enabled = false },
			[vim.diagnostic.severity.INFO] = { enabled = false },
			[vim.diagnostic.severity.HINT] = { enabled = true },
		},

		-- If set, the letters for each buffer in buffer-pick mode will be
		-- assigned based on their name. Otherwise or in case all letters are
		-- already assigned, the behavior is to assign letters in order of
		-- usability (see order below)
		semantic_letters = true,

		-- New buffer letters are assigned in this order. This order is
		-- optimal for the qwerty keyboard layout but might need adjustement
		-- for other layouts.
		letters = "asdfjklghnmxcvbziowerutyqpASDFJKLGHNMXCVBZIOWERUTYQP",
	})
end

return M
