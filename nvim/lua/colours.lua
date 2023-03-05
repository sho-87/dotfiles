-- set default Todo HL to same as Comment (plugins can use their own HL group for Todo:)
vim.api.nvim_set_hl(0, "Todo", { link = "Comment" })

local C = {}

local function set_theme_colours(theme)
	if theme == "rose-pine" then
		local theme_colors = require("rose-pine.palette")
		C.normal = "#af88dd"
		C.insert = theme_colors.gold
		C.visual = theme_colors.foam
		C.command = theme_colors.rose
		C.replace = theme_colors.love
		C.text = theme_colors.text
		C.comment = theme_colors.muted
		C.overlay = theme_colors.overlay
		C.sep = theme_colors.subtle
	elseif theme == "kanagawa" then
		local colors = require("kanagawa.colors").setup()
		local theme_colors = colors.palette
		C.normal = theme_colors.autumnRed
		C.insert = theme_colors.autumnYellow
		C.visual = theme_colors.dragonBlue
		C.command = theme_colors.autumnRed
		C.replace = theme_colors.autumnYellow
		C.text = theme_colors.fujiGray
		C.comment = theme_colors.fujiGray
		C.overlay = theme_colors.crystalBlue
		C.sep = theme_colors.carpYellow
	end
end

local current_theme = vim.api.nvim_exec("color", true)
set_theme_colours(current_theme)

return C
