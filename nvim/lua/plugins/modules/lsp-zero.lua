local M = {
	"VonHeikemen/lsp-zero.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	branch = "v1.x",
	dependencies = {
		-- LSP Support
		{ "neovim/nvim-lspconfig" }, -- Required
		{ "williamboman/mason.nvim" }, -- Optional
		{ "williamboman/mason-lspconfig.nvim" }, -- Optional
		{ "jay-babu/mason-null-ls.nvim" },
		{ "onsails/lspkind.nvim" },
		{ "jose-elias-alvarez/null-ls.nvim", dependencies = "nvim-lua/plenary.nvim" },

		-- Autocompletion
		{ "hrsh7th/nvim-cmp" }, -- Required
		{ "hrsh7th/cmp-nvim-lsp" }, -- Required
		{ "hrsh7th/cmp-buffer" }, -- Optional
		{ "hrsh7th/cmp-path" }, -- Optional
		{ "saadparwaiz1/cmp_luasnip" }, -- Optional
		{ "hrsh7th/cmp-nvim-lua" }, -- Optional

		-- Snippets
		{ "L3MON4D3/LuaSnip" }, -- Required
		{ "rafamadriz/friendly-snippets" }, -- Optional
	},
	event = { "VeryLazy" },
}

function M.config()
	local lsp = require("lsp-zero").preset({
		name = "recommended",
		set_lsp_keymaps = false,
		manage_nvim_cmp = true,
		suggest_lsp_servers = true,
	})

	local has_words_before = function()
		if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then
			return false
		end
		local line, col = unpack(vim.api.nvim_win_get_cursor(0))
		return col ~= 0 and vim.api.nvim_buf_get_text(0, line - 1, 0, line - 1, col, {})[1]:match("^%s*$") == nil
	end

	local cmp = require("cmp")
	local lspkind = require("lspkind")

	lsp.setup_nvim_cmp({
		sources = {
			{ name = "copilot", keyword_length = 0 },
			{ name = "nvim_lsp", keyword_length = 1 },
			{ name = "path" },
			{ name = "buffer", keyword_length = 3 },
			{ name = "luasnip", keyword_length = 2 },
		},
		formatting = {
			format = lspkind.cmp_format({
				mode = "symbol_text",
				max_width = 50,
				symbol_map = { Copilot = "" },
			}),
		},
		mapping = {
			["<CR>"] = cmp.mapping.confirm({ select = true }),
			["<C-Space>"] = cmp.mapping.complete(),
			["<C-e>"] = cmp.mapping({
				i = cmp.mapping.abort(),
				c = cmp.mapping.close(),
			}),
			["<Tab>"] = vim.schedule_wrap(function(fallback)
				if cmp.visible() and has_words_before() then
					cmp.select_next_item({ behavior = cmp.SelectBehavior })
				else
					fallback()
				end
			end),
			["<S-Tab>"] = vim.schedule_wrap(function(fallback)
				if cmp.visible() and has_words_before() then
					cmp.select_prev_item({ behavior = cmp.SelectBehavior })
				else
					fallback()
				end
			end),
		},
	})

	lsp.on_attach(map_lsp(client, bufnr))

	-- Configure lua language server for neovim
	lsp.nvim_workspace()
	lsp.setup()

	vim.diagnostic.config({
		virtual_text = { spacing = 20, severity = { min = vim.diagnostic.severity.WARN } },
		signs = true,
		update_in_insert = false,
		underline = false,
		severity_sort = true,
		float = {
			focusable = true,
			border = "rounded",
			source = "always",
		},
	})

	-- Configure null-ls
	local null_ls = require("null-ls")
	local null_opts = lsp.build_options("null-ls", {})

	null_ls.setup({
		on_attach = function(client, bufnr)
			null_opts.on_attach(client, bufnr)

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
		sources = {},
	})

	-- Make null-ls aware of the tools installed using mason.nvim, and configure them automatically.
	require("mason-null-ls").setup({
		ensure_installed = nil,
		automatic_installation = true,
		automatic_setup = true,
	})
	require("mason-null-ls").setup_handlers()
end

return M
