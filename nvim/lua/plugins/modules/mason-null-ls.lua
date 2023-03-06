-- Integrate Mason with null-ls
local M = {
	"jay-babu/mason-null-ls.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = {
		"williamboman/mason.nvim",
		"williamboman/mason-lspconfig.nvim",
		{ "jose-elias-alvarez/null-ls.nvim", dependencies = "nvim-lua/plenary.nvim" },
	},
	event = "VeryLazy",
}

function M.config()
	require("mason").setup()
	require("mason-null-ls").setup({
		ensure_installed = {
			-- lsp
			"docker-compose-language-service",
			"lua-language-server",
			"vim-language-server",
			"pyright",
			"typescript-language-server",

			-- linters
			"ruff",
			"markdownlint",
			"eslint_d",

			-- formatters
			"black",
			"isort",
			"stylua",
			"prettierd",
		},
		automatic_installation = false,
		automatic_setup = true,
	})
	require("null-ls").setup({
		on_attach = function(client, bufnr)
			-- Custom command to use null-ls as the formatter.
			local format_cmd = function(input)
				vim.lsp.buf.format({
					id = client.id,
					timeout_ms = 5000,
					async = input.bang,
				})
			end

			local bufcmd = vim.api.nvim_buf_create_user_command
			bufcmd(bufnr, "NullFormat", format_cmd, {
				bang = true,
				range = true,
			})
		end,
		sources = {
			-- Anything not supported by mason.
		},
	})
	require("mason-null-ls").setup_handlers()
end

return M
