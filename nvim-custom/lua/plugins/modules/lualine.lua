local M = {
	"nvim-lualine/lualine.nvim",
	enabled = true,
    cond = vim.g.vscode == nil,
	dependencies = { "nvim-tree/nvim-web-devicons" },
	event = { "BufReadPost", "BufNewFile" },
}

function M.config()
	local utils = require("utils")
	local colours = require("colours")
	local custom = require("lualine.themes.base16")
	custom.normal.a.bg = colours.normal
	custom.insert.a.bg = colours.insert
	custom.visual.a.bg = colours.visual
	custom.command.a.bg = colours.command
	custom.replace.a.bg = colours.replace

	require("lualine").setup({
		options = {
			theme = custom,
			globalstatus = true,
			section_separators = "",
			component_separators = "",
			disabled_filetypes = {
				statusline = { "alpha" },
				winbar = { "neo-tree", "aerial", "OverseerList", "alpha" },
			},
			refresh = {
				statusline = 1000,
				tabline = 2000,
				winbar = 500,
			},
		},
		sections = {
			lualine_a = {
				{ "mode", padding = 2 },
			},
			lualine_b = {
				{
					function()
						return " "
					end,
					cond = utils.is_git_repo,
					color = colours.status_icon,
					separator = { right = "" },
				},
				{
					"branch",
					icon = "",
					padding = { left = 0, right = 2 },
					color = { fg = require("colours").textLight, bg = colours.status },
					on_click = function()
						require("telescope.builtin").git_branches()
					end,
				},
				{
					"diff",
					symbols = { added = " ", modified = " ", removed = " " },
					color = { bg = colours.status },
					padding = { left = 0, right = 2 },
					on_click = function()
						require("telescope.builtin").git_status()
					end,
				},
				{
					function()
						return "⚐"
					end,
					color = colours.status_icon,
					separator = { left = "", right = "" },
					cond = function()
						if #vim.diagnostic.get(0, { severity = { min = vim.diagnostic.severity.HINT } }) > 0 then
							return true
						else
							return false
						end
					end,
				},
				{
					"diagnostics",
					color = { bg = colours.status },
					padding = { left = 1, right = 2 },
					on_click = function()
						vim.cmd("TroubleToggle")
					end,
				},
			},
			lualine_c = {
				{
					function()
						return "⚒"
					end,
					color = colours.status_icon,
					cond = function()
						local tasks = require("overseer").list_tasks()
						if vim.tbl_isempty(tasks) then
							return false
						else
							return true
						end
					end,
					separator = { left = "", right = "" },
				},
				{
					"overseer",
					on_click = function()
						vim.cmd("OverseerToggle")
					end,
				},
			},
			lualine_x = {},
			lualine_y = {
				{
					"filetype",
					colored = false,
					color = { fg = colours.textLight, bg = colours.status },
					icon = { align = "left" },
					on_click = function()
						vim.ui.input({
							prompt = "Enter new filetype: ",
							default = vim.bo.filetype,
							completion = "filetype",
						}, function(input)
							if input ~= nil and input ~= "" then
								vim.cmd("set filetype=" .. input)
							end
						end)
					end,
				},
				{
					function()
						local msg = "No active LSP"
						local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
						local clients = vim.lsp.get_active_clients()
						if next(clients) == nil then
							return msg
						end
						for _, client in ipairs(clients) do
							local filetypes = client.config.filetypes
							if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
								return client.name
							end
						end
						return msg
					end,
					color = { fg = colours.textLight, bg = colours.status },
					padding = { left = 1, right = 2 },
					icon = " ",
					on_click = function()
						vim.cmd("LspInfo")
					end,
				},
			},
			lualine_z = {
				{
					function()
						return ""
					end,
					color = colours.status_icon,
					separator = { left = "" },
				},
				{
					"location",
					padding = { left = 1, right = 0 },
					fmt = function(str, _)
						local loc = vim.split(str, ":")
						return string.format("%d/%d", loc[1], vim.api.nvim_buf_line_count(0))
					end,
					on_click = function()
						vim.ui.input({
							prompt = "Go to line: ",
							default = vim.fn.line("."),
						}, function(input)
							if input ~= nil and input ~= "" then
								vim.cmd("normal! " .. input .. "G")
							end
						end)
					end,
				},
				{
					"progress",
					padding = 1,
					fmt = function(str, _)
						return string.format(": %s", str)
					end,
				},
			},
		},
		inactive_sections = {
			lualine_a = {},
			lualine_b = {},
			lualine_c = {},
			lualine_x = { { "filetype", color = { fg = "grey" }, colored = false } },
			lualine_y = {},
			lualine_z = {},
		},
		winbar = {
			lualine_a = {},
			lualine_b = {},
			lualine_c = {
				{ "filetype", colored = true, icon_only = true },
				{
					"filename",
					color = { fg = colours.bufSelected },
					symbols = {
						modified = "*", -- Text to show when the file is modified.
						readonly = "(RO)", -- Text to show when the file is non-modifiable or readonly.
					},
					padding = { left = 0, right = 4 },
				},
				{
					"aerial",
					depth = 3,
					on_click = function()
						vim.cmd("AerialToggle")
					end,
				},
			},
			lualine_x = {},
			lualine_y = {},
			lualine_z = {},
		},
		inactive_winbar = {
			lualine_a = {},
			lualine_b = {},
			lualine_c = {
				{
					"filetype",
					colored = false,
					icon_only = true,
					color = { fg = colours.bufVisible },
				},
				{
					"filename",
					color = { fg = colours.bufVisible },
					symbols = {
						modified = "", -- Text to show when the file is modified.
						readonly = "(RO)", -- Text to show when the file is non-modifiable or readonly.
					},
					padding = { left = 0, right = 4 },
				},
			},
			lualine_x = {},
			lualine_y = {},
			lualine_z = {},
		},
		extensions = {
			"aerial",
            "lazy",
			"man",
			"neo-tree",
			"quickfix",
			"toggleterm",
            "trouble",
			"overseer",
		},
	})
end

return M
