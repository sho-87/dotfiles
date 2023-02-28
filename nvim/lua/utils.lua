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

-- Get colour of the mode
local colours = require("colours")
local function get_mode_colour()
	local mode = vim.api.nvim_get_mode()["mode"]

	if mode == "n" then
		return { fg = colours.normal }
	elseif mode == "i" then
		return { fg = colours.insert }
	elseif mode == "v" then
		return { fg = colours.visual }
	elseif mode == "V" then
		return { fg = colours.visual }
	elseif mode == "R" then
		return { fg = colours.replace }
	elseif mode == "c" then
		return { fg = colours.command }
	else
		return { fg = colours.text }
	end
end
M.get_mode_colour = get_mode_colour

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

local function live_grep_from_project_git_root()
	local function is_git_repo()
		vim.fn.system("git rev-parse --is-inside-work-tree")
		return vim.v.shell_error == 0
	end

	local function get_git_root()
		local dot_git_path = vim.fn.finddir(".git", ".;")
		return vim.fn.fnamemodify(dot_git_path, ":h")
	end

	local opts = {}

	if is_git_repo() then
		opts = {
			cwd = get_git_root(),
		}
	end

	require("telescope.builtin").live_grep(opts)
end
M.live_grep_from_project_git_root = live_grep_from_project_git_root

return M
