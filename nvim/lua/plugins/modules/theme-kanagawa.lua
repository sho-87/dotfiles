-- ╭─────────────────────────────────────────────────╮
-- │ Remember to run :KanagawaCompile after editing! │
-- ╰─────────────────────────────────────────────────╯

local M = {
	"rebelot/kanagawa.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	lazy = false, -- make sure we load this during startup
	priority = 1000, -- make sure to load this before all the other start plugins
}

function M.config()
	local wave = require("kanagawa.colors").setup({ theme = "wave" }) -- borrow colours from wave theme
	local wave_theme = wave.theme
	local palette = wave.palette

	require("kanagawa").setup({
		undercurl = true,
		commentStyle = { italic = false },
		functionStyle = { italic = false, bold = true },
		keywordStyle = { italic = false, bold = true },
		statementStyle = { italic = false, bold = true },
		typeStyle = { italic = false },
		transparent = false,
		dimInactive = true, -- dim inactive window `:h hl-NormalNC`
		terminalColors = true, -- define vim.g.terminal_color_{0,17}
		colors = {
			palette = {},
			theme = {
				wave = {},
				lotus = {},
				dragon = {
					ui = { -- use wave theme ui colors
						fg = wave_theme.ui.fg,
						fg_dim = wave_theme.ui.fg_dim,
						fg_reverse = wave_theme.ui.fg_reverse,

						bg_dim = wave_theme.ui.bg_dim,
						bg_m3 = wave_theme.ui.bg_m3,
						bg_m2 = wave_theme.ui.bg_m2,
						bg_m1 = wave_theme.ui.bg_m1,
						bg = wave_theme.ui.bg,
						bg_p1 = wave_theme.ui.bg_p1,
						bg_p2 = wave_theme.ui.bg_p2,

						special = wave_theme.ui.special,
						nontext = wave_theme.ui.nontext,
						whitespace = wave_theme.ui.whitespace,

						bg_search = wave_theme.ui.bg_search,
						bg_visual = wave_theme.ui.bg_visual,

						pmenu = {
							fg = wave_theme.ui.pmenu.fg,
							bg = wave_theme.ui.pmenu.bg,
							bg_sel = wave_theme.ui.pmenu.bg_sel,
							bg_sbar = wave_theme.ui.pmenu.bg_sbar,
							bg_thumb = wave_theme.ui.pmenu.bg_thumb,
						},
						float = {
							fg = wave_theme.ui.float.fg,
							bg = wave_theme.ui.float.bg,
							fg_border = wave_theme.ui.float.fg_border,
							bg_border = wave_theme.ui.float.bg_border,
						},
					},
				},
				all = { ui = { bg_gutter = "none", pmenu = { fg_sel = "none" } } },
			},
		},
		overrides = function(colors) -- add/modify highlights
			return {
				DiagnosticVirtualTextError = { fg = colors.palette.samuraiRed },
				DiagnosticVirtualTextWarn = { fg = colors.palette.roninYellow },
				DiagnosticVirtualTextInfo = { fg = colors.palette.waveAqua1 },
				DiagnosticVirtualTextHint = { fg = colors.palette.dragonBlue },
				IndentBlanklineChar = { fg = colors.palette.sumiInk4 },

				-- NormalNC = { fg = colors.palette.dragonWhite },
				-- Directory = { fg = colors.palette.carpYellow },
				-- SignColumn = { guibg = NONE },

				-- BufferCurrent = { bg = "overlay", fg = "text" },
				-- BufferCurrentTarget = { fg = colors.palette.autumnRed },
				-- BufferCurrentSign = { bg = "overlay" },
				-- BufferCurrentMod = { bg = "overlay" },
				-- BufferInactive = { bg = "overlay", fg = "text" },
				-- BufferInactiveTarget = { fg = colors.palette.autumnRed },
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
				NeoTreeIndentMarker = { link = "IndentBlanklineChar" },

				-- JupyniumCodeCellSeparator = { bg = colors.palette.waveBlue2 },
				-- JupyniumMarkdownCellSeparator = { bg = colors.palette.waveBlue2 },
				-- JupyniumMarkdownCellContent = { bg = colors.palette.sumiInk4 },
				-- JupyniumMagicCommand = { link = "Keyword" },

				ScrollbarCursor = { fg = colors.palette.oldWhite },
				WhichKey = { fg = colors.palette.peachRed },
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
