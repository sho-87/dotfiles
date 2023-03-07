-- Integrate Mason with nvim lsp and null-ls
local M = {
	"williamboman/mason-lspconfig.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = {
		"neovim/nvim-lspconfig",
		"williamboman/mason.nvim",
		"jay-babu/mason-null-ls.nvim",
		"hrsh7th/cmp-nvim-lsp",
		{ "jose-elias-alvarez/null-ls.nvim", dependencies = "nvim-lua/plenary.nvim" },
	},
	event = "VeryLazy",
}

function M.config()
	-- setup mason for tool installation
	require("mason").setup()

	-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
	local capabilities = vim.lsp.protocol.make_client_capabilities()
	capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

	-- settings for lsp servers
	local servers = {
		lua_ls = {
			Lua = {
				workspace = { checkThirdParty = false },
				telemetry = { enable = false },
				diagnostics = {
					globals = { "vim" },
				},
			},
		},
	}

	-- on_attach function to be added to each server
	local on_attach = function(_, bufnr)
		-- create buffer local autocommands to show and hide virtual diagnostic text
		local vt = {
			spacing = 10,
			severity = { min = vim.diagnostic.severity.WARN },
			format = function(diagnostic)
				local severity_letter = string.sub(vim.diagnostic.severity[diagnostic.severity], 1, 1)
				local msg = diagnostic.message
				local msg_threshold = 100

				local num_windows = #vim.api.nvim_tabpage_list_wins(0) / 2
				if num_windows > 1 then
					msg_threshold = msg_threshold / num_windows
				end

				if string.len(msg) > msg_threshold then
					msg = string.sub(msg, 1, msg_threshold) .. "..."
				end

				return string.format("%s: %s", severity_letter, msg)
			end,
		}
		vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
			buffer = bufnr,
			callback = function()
				vim.diagnostic.config({ virtual_text = vt })
			end,
		})
		vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
			buffer = bufnr,
			callback = function()
				vim.diagnostic.config({ virtual_text = false })
			end,
		})

		-- map buffer local keys once lsp is attached
		MapLSP(bufnr)
	end

	-- define the function used to set up the installed servers
	require("mason-lspconfig").setup_handlers({
		function(server_name)
			require("lspconfig")[server_name].setup({
				capabilities = capabilities,
				settings = servers[server_name],
				on_attach = on_attach,
			})
		end,
	})

	-- setup lsp diagnostic signs
	vim.fn.sign_define("DiagnosticSignError", { texthl = "DiagnosticSignError", text = "", numhl = "" })
	vim.fn.sign_define("DiagnosticSignWarn", { texthl = "DiagnosticSignWarn", text = "", numhl = "" })
	vim.fn.sign_define("DiagnosticSignHint", { texthl = "DiagnosticSignHint", text = "", numhl = "" })
	vim.fn.sign_define("DiagnosticSignInfo", { texthl = "DiagnosticSignInfo", text = "", numhl = "" })

	-- defined mason tools to be installed and set up
	require("mason-null-ls").setup({
		ensure_installed = {
			-- lsp
			"docker-compose-language-service",
			"lua-language-server",
			"vim-language-server",
			"ruff-lsp",
			"typescript-language-server",

			-- linters
			"ruff",
			"markdownlint",
			"eslint_d",

			-- formatters
			"black",
			"stylua",
			"prettierd",
		},
		automatic_installation = false,
		automatic_setup = true,
	})

	-- setup null-ls
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

	-- setup handlers for mason tools
	require("mason-null-ls").setup_handlers()
end

return M
