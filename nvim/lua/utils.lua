local M = {}

-- Functional wrapper for mapping custom keybindings
local function map(mode, lhs, rhs, opts)
	local options = { noremap = true, silent = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.keymap.set(mode, lhs, rhs, options)
end
M.map = map

-- open toggleterm with lazygit
local Terminal = require("toggleterm.terminal").Terminal
local lazygit = Terminal:new({
	cmd = "lazygit",
	hidden = true,
	on_open = function(term)
		vim.cmd("startinsert!")
	end,
	close_on_exit = true,
	direction = "float",
	float_opts = {
		border = "single",
	},
})

local function toggle_lazygit()
	lazygit:toggle()
end
M.toggle_lazygit = toggle_lazygit

return M
