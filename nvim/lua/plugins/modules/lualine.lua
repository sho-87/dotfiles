local M = {
	"nvim-lualine/lualine.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = { "nvim-tree/nvim-web-devicons" },
	event = { "VeryLazy" },
}

function M.config()
	local colours = require("colours")
	local custom = require("lualine.themes.dracula")
	custom.normal.a.bg = colours.normal
	custom.insert.a.bg = colours.insert
	custom.visual.a.bg = colours.visual
	custom.command.a.bg = colours.command
	custom.replace.a.bg = colours.replace

	require("lualine").setup({
		options = {
			theme = custom,
			globalstatus = true,
		},
		sections = {
			lualine_a = { { "mode", padding = 2 } },
			lualine_b = {
				{ "branch", color = require("utils").get_mode_colour },
				"diff",
				{
					"diagnostics",
					on_click = function()
						vim.cmd("TroubleToggle")
					end,
				},
			},
			lualine_c = {
				{
					"aerial",
					depth = 2,
					on_click = function()
						vim.cmd("AerialToggle")
					end,
				},
			},
			lualine_x = { "encoding", "fileformat" },
			lualine_y = { "filetype" },
			lualine_z = { "location", { "progress", padding = 2 } },
		},
		inactive_sections = {
			lualine_a = {},
			lualine_b = {},
			lualine_c = {},
			lualine_x = { { "filetype", color = { fg = "grey" }, colored = false } },
			lualine_y = {},
			lualine_z = {},
		},
		-- extensions = { "neo-tree", "aerial" }, -- off if using globalstatus
	})
end

return M
