local M = {
	"simrat39/rust-tools.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = { "neovim/nvim-lspconfig", "nvim-lua/plenary.nvim", "mfussenegger/nvim-dap" },
	ft = "rust",
}

function M.config()
	local rt = require("rust-tools")

	rt.setup({
		server = {
			on_attach = function(_, bufnr)
				-- Hover actions
				vim.keymap.set("n", "<leader>gh", rt.hover_actions.hover_actions, { buffer = bufnr })
				-- Code action groups
				vim.keymap.set("n", "<leader>gA", rt.code_action_group.code_action_group, { buffer = bufnr })
			end,
		},
	})
end

return M
