-- Integrate Mason with nvim-dap
local M = {
	"jay-babu/mason-nvim-dap.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = {
		"williamboman/mason.nvim",
		"mfussenegger/nvim-dap",
		"mfussenegger/nvim-dap-python",
	},
	event = "VeryLazy",
}

function M.config()
	require("mason").setup()
	require("mason-nvim-dap").setup({
		ensure_installed = {
			"python",
		},
		automatic_installation = false,
		automatic_setup = true,
	})
	require("mason-nvim-dap").setup_handlers()
    require("dap-python").setup("~/.virtualenv/debugpy/Scripts/python.exe")
end
return M
