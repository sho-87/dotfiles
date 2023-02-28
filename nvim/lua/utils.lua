local M = {}

-- Functional wrapper for mapping custom keybindings
function M.map(mode, lhs, rhs, opts)
	local options = { noremap = true, silent = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.keymap.set(mode, lhs, rhs, options)
end

-- Get colour of the mode
local colours = require("colours")
function M.get_mode_colour()
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

function M.toggle_lazygit()
	lazygit:toggle()
end

-- when grepping, cd to the project root directory first
function M.live_grep_from_project_git_root()
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

-- luasnip: insert visual selection into dynamic node
local ls = require("luasnip")
local sn = ls.snippet_node
local i = ls.insert_node
local f = ls.function_node

function M.get_visual(parent)
	print("Creating snippet from visual selection...")
	if #parent.snippet.env.SELECT_RAW > 0 then
		return sn(nil, i(1, parent.snippet.env.SELECT_RAW))
	else
		return sn(nil, i(1, ""))
	end
end

-- luasnip: return the regex capture group for regex-based triggers
function M.get_capture_group(group)
	return f(function(_, snip)
		return snip.captures[group]
	end)
end

return M
