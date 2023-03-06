local M = {
	"VonHeikemen/lsp-zero.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	branch = "v1.x",
	dependencies = "neovim/nvim-lspconfig", -- Required
	event = { "VeryLazy" },
}

function M.config()
	local lsp = require("lsp-zero").preset({
		name = "recommended",
		set_lsp_keymaps = false,
		manage_nvim_cmp = false,
		manage_luasnip = false,
		suggest_lsp_servers = true,
	})

	lsp.on_attach(function(client, bufnr)
		-- create buffer local autocommand to show and hide virtual diagnostic text
		local vt = {
			spacing = 10,
			severity = { min = vim.diagnostic.severity.HINT },
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
		map_lsp(bufnr)
	end)

	-- Configure lua language server for neovim
	lsp.nvim_workspace()
	lsp.setup()

	vim.diagnostic.config({
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
end

return M
