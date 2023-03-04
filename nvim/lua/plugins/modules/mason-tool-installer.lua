local M = {
	"WhoIsSethDaniel/mason-tool-installer.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = {
		{ "williamboman/mason.nvim" },
	},
	lazy = false,
}

function M.config()
	require("mason-tool-installer").setup({
		ensure_installed = {
			-- lsp
			"docker-compose-language-service",
			"lua-language-server",
			"vim-language-server",
			"pyright",
			"typescript-language-server",

			-- linters
			"markdownlint",
			"eslint_d",

			-- formatters
			"black",
			"isort",
			"stylua",
			"prettierd",
		},

		-- if set to true this will check each tool for updates. If updates
		-- are available the tool will be updated. This setting does not
		-- affect :MasonToolsUpdate or :MasonToolsInstall.
		-- Default: false
		auto_update = true,

		-- automatically install / update on startup. If set to false nothing
		-- will happen on startup. You can use :MasonToolsInstall or
		-- :MasonToolsUpdate to install tools and check for updates.
		-- Default: true
		run_on_start = true,
	})
end

return M
