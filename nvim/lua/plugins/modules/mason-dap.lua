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
            "codelldb",
			"python",
		},
        automatic_setup = true,
	})

	-- path to mason-installed debugpy and python
	local path_debugpy = vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/Scripts/python.exe"
	-- local path_python = vim.env.HOME .. "/miniconda3/python.exe"
    local path_python = print(vim.api.nvim_exec("!which python", true))

	local dap = require("dap")

	require("mason-nvim-dap").setup_handlers({
		-- function(source_name)
		-- 	-- all sources with no handler get passed here
		-- 	-- Keep original functionality of `automatic_setup = true`
		-- 	require("mason-nvim-dap.automatic_setup")(source_name)
		-- end,
		-- python = function(source_name)
		-- 	dap.adapters.python = {
		-- 		type = "executable",
		-- 		command = path_python,
		-- 		args = {
		-- 			"-m",
		-- 			"debugpy.adapter",
		-- 		},
		-- 	}
		-- end,
	})

	-- setup mason-installed debugpy and python
	require("dap-python").setup(path_debugpy)
	require("dap-python").resolve_python = function()
		return path_python
	end

	-- set debugger signs
	vim.fn.sign_define("DapBreakpoint", { text = "", texthl = "DapBreakpoint", linehl = "", numhl = "" })
	vim.fn.sign_define("DapBreakpointCondition", { text = "ﳁ", texthl = "DapBreakpoint", linehl = "", numhl = "" })
	vim.fn.sign_define("DapBreakpointRejected", { text = "", texthl = "DapBreakpoint", linehl = "", numhl = "" })
	vim.fn.sign_define("DapLogPoint", { text = "", texthl = "DapLogPoint", linehl = "", numhl = "" })
	vim.fn.sign_define("DapStopped", { text = "", texthl = "DapStopped", linehl = "", numhl = "" })
end
return M
