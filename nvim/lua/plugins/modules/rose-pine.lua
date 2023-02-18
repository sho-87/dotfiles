local M = {
	"rose-pine/neovim",
	name = "rose-pine",
	cond = vim.g.vscode == nil,
	enabled = true,
	lazy = false, -- make sure we load this during startup if it is your main colorscheme
	priority = 1000, -- make sure to load this before all the other start plugins
}

function M.config()
	require("rose-pine").setup({
		dark_variant = "moon",
		bold_vert_split = false,
		-- dim_nc_background = true,
		disable_italics = true,
		highlight_groups = {
			IndentBlanklineChar = { fg = "highlight_low" },
			NormalFloat = { bg = "surface" },
			Pmenu = { bg = "surface" },
			SignColumn = { guibg = NONE },
		},
	})

	-- load the colorscheme after config
	vim.cmd([[colorscheme rose-pine]])
end

return M
