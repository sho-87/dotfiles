local M = {
	"rebelot/kanagawa.nvim",
	cond = vim.g.vscode == nil,
	enabled = false,
	lazy = false, -- make sure we load this during startup
	priority = 1000, -- make sure to load this before all the other start plugins
}

function M.config()
	require("kanagawa").setup({
		undercurl = true, -- enable undercurls
		commentStyle = { italic = true },
		functionStyle = { italic = false },
		keywordStyle = { italic = false },
		statementStyle = { italic = false, bold = true },
		typeStyle = { italic = false },
		transparent = false, -- do not set background color
		dimInactive = true, -- dim inactive window `:h hl-NormalNC`
		terminalColors = true, -- define vim.g.terminal_color_{0,17}
		colors = { -- add/modify theme and palette colors
			palette = {},
			theme = { wave = {}, lotus = {}, dragon = {}, all = {} },
		},
		overrides = function(colors) -- add/modify highlights
			return {
				DiagnosticVirtualTextError = { fg = colors.palette.autumnRed },
				DiagnosticVirtualTextWarn = { fg = colors.palette.boatYellow1 },
				DiagnosticVirtualTextInfo = { fg = colors.palette.dragonBlue },
				DiagnosticVirtualTextHint = { fg = colors.palette.springViolet1 },
				-- Directory = { fg = colors.palette.carpYellow },
				-- NormalFloat = { bg = "overlay" },
				-- Pmenu = { bg = "surface" },
				IndentBlanklineChar = { fg = colors.palette.winterYellow },
				SignColumn = { guibg = NONE },

				-- BufferCurrent = { bg = "overlay", fg = "text" },
				BufferCurrentTarget = { fg = colors.palette.autumnRed },
				-- BufferCurrentSign = { bg = "overlay" },
				-- BufferCurrentMod = { bg = "overlay" },
				-- BufferInactive = { bg = "overlay", fg = "text" },
				BufferInactiveTarget = { fg = colors.palette.autumnRed },
				-- BufferInactiveSign = { bg = "overlay" },
				-- BufferInactiveMod = { bg = "overlay" },

				hl_incline = { bg = colors.palette.fujiWhite, fg = colors.palette.sumiInk0 },

				-- Headline1 = { bg = "pine" },
				-- Headline2 = { bg = "#345663" },
				-- Dash = { bg = "love" },
				-- Quote = { bg = "love" },

				-- LeapLabelPrimary = { fg = "base", bg = "iris" },
				-- LeapLabelSecondary = { fg = "base", bg = "love" },
				-- NeoTreeRootName = { fg = "love" },
				-- NeoTreeIndentMarker = { fg = "overlay" },

				JupyniumCodeCellSeparator = { bg = colors.palette.waveBlue2 },
				JupyniumMarkdownCellSeparator = { bg = colors.palette.waveBlue2 },
				JupyniumMarkdownCellContent = { bg = colors.palette.sumiInk4 },
				JupyniumMagicCommand = { link = "Keyword" },

				-- Scrollbar = { bg = "highlight_med" },
				-- ScrollbarCursor = { fg = "foam" },

				-- WhichKey = { fg = "love" },
				-- WhichKeyGroup = { fg = "subtle" },
				-- WhichKeyFloat = { bg = "overlay" },
				YankyYanked = { bg = colors.palette.winterYellow },
				YankyPut = { bg = colors.palette.winterRed },
			}
		end,
		theme = "dragon",
		background = { -- map the value of 'background' option to a theme
			dark = "dragon",
			light = "lotus",
		},
	})

	-- load the colorscheme after config
	vim.cmd([[colorscheme kanagawa-dragon]])
end

return M
