local M = {
	"b0o/incline.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	event = { "BufReadPost", "BufNewFile" },
}

function M.config()
	require("incline").setup({
		hide = {
			cursorline = false,
			focused_win = false,
			only_win = false,
		},
		highlight = {
			groups = {
				InclineNormal = {
					default = true,
					group = "Cursor",
				},
				InclineNormalNC = {
					default = true,
					group = "NormalFloat",
				},
			},
		},
		ignore = {
			-- "acwrite",
			-- "help",
			-- "nofile",
			-- "nowrite",
			-- "quickfix",
			-- "terminal",
			-- "prompt",
			buftypes = { "nofile", "prompt" },
			floating_wins = true,
			unlisted_buffers = false,
			-- 'autocmd',
			-- 'command',
			-- 'loclist',
			-- 'popup',
			-- 'preview',
			-- 'quickfix',
			-- 'unknown',
			wintypes = "special",
		},
		render = function(props)
			local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(props.buf), ":t")
			local icon, color = require("nvim-web-devicons").get_icon_color(filename)
			return {
				{ icon, guifg = color },
				{ " " },
				{ filename },
			}
		end,
		window = {
			margin = {
				horizontal = 2,
				vertical = 1,
			},
			options = {
				signcolumn = "no",
				wrap = false,
			},
			padding = 1,
			padding_char = " ",
			placement = {
				horizontal = "right",
				vertical = "top",
			},
			width = "fit",
			winhighlight = {
				active = {
					EndOfBuffer = "None",
					Normal = "InclineNormal",
					Search = "None",
				},
				inactive = {
					EndOfBuffer = "None",
					Normal = "InclineNormalNC",
					Search = "None",
				},
			},
			zindex = 50,
		},
	})
end

return M
