local map = require("utils").map
local vscode = vim.g.vscode

-- General
map("n", "<esc>", "<cmd>nohlsearch<cr>", { desc = "Clear highlights" }) -- Clear highlights on ESC
map("n", "cd", ":cd %:p:h<cr>:pwd<cr>", { desc = "Change working directory" }) -- Change directory to current file's directory
map("n", "<leader>qq", "<cmd>qa<cr>") -- Quit all windows

-- Undo (rest are in telescope module)
if not vscode then
	map("n", "<leader>u", "<cmd>Telescope undo<cr>", { desc = "Undo tree" })
end

-- Help
if vscode then
	map("n", "<leader>?k", '<cmd>call VSCodeNotify("workbench.action.keybindingsReference")<cr>')
else
	map("n", "<leader>?", "{}", { desc = "Help" })
	map("n", "<leader>?h", "<cmd>lua require('telescope.builtin').help_tags()<cr>", { desc = "Help" })
	map("n", "<leader>?k", "<cmd>lua require('telescope.builtin').keymaps()<cr>", { desc = "Keymaps" })
	map("n", "<leader>?c", "<cmd>lua require('telescope.builtin').commands()<cr>", { desc = "Commands" })
	map("n", "<leader>?a", "<cmd>lua require('telescope.builtin').autocommands()<cr>", { desc = "Autocommands" })
	map("n", "<leader>?g", "<cmd>lua require('telescope.builtin').highlights()<cr>", { desc = "Highlight groups" })
	map("n", "<leader>?v", "<cmd>lua require('telescope.builtin').vim_options()<cr>", { desc = "Vim options" })
	map("n", "<leader>?n", "<cmd>Telescope noice<cr>", { desc = "Notifications" })
	map("n", "<leader>?s", "<cmd>Telescope luasnip<CR>", { desc = "Snippets" })
	map("n", "<leader>?S", "<cmd>lua require('luasnip.loaders').edit_snippet_files()<cr>", { desc = "Edit snippets" })
end

-- Tools
if not vscode then
	map("n", "<leader>z", "{}", { desc = "Tools" })
	map("n", "<leader>zc", "<cmd>CccPick<cr>", { desc = "Colour picker" })
	map("n", "<leader>zl", "<cmd>Lazy<cr>", { desc = "Lazy" })
	map("n", "<leader>zm", "<cmd>Mason<cr>", { desc = "Mason" })
	map("n", "<leader>zs", "<cmd>StartupTime<cr>", { desc = "StartupTime" })
	map("n", "<leader>zr", "<cmd>luafile %<CR>", { desc = "Source current file" })
end

-- Splits
if vscode then
	map("n", "<leader>wv", '<cmd>call VSCodeNotify("workbench.action.splitEditorRight")<cr>')
	map("n", "<leader>ws", '<cmd>call VSCodeNotify("workbench.action.splitEditorDown")<cr>')
	map("n", "<leader>wq", '<cmd>call VSCodeNotify("workbench.action.closeActiveEditor")<cr>')

	map("n", "<leader>wh", '<cmd>call VSCodeNotify("workbench.action.focusLeftGroup")<cr>')
	map("n", "<leader>wj", '<cmd>call VSCodeNotify("workbench.action.focusBelowGroup")<cr>')
	map("n", "<leader>wk", '<cmd>call VSCodeNotify("workbench.action.focusAboveGroup")<cr>')
	map("n", "<leader>wl", '<cmd>call VSCodeNotify("workbench.action.focusRightGroup")<cr>')

	map("n", "<leader>wH", '<cmd>call VSCodeNotify("workbench.action.moveEditorToLeftGroup")<cr>')
	map("n", "<leader>wJ", '<cmd>call VSCodeNotify("workbench.action.moveEditorToBelowGroup")<cr>')
	map("n", "<leader>wK", '<cmd>call VSCodeNotify("workbench.action.moveEditorToAboveGroup")<cr>')
	map("n", "<leader>wL", '<cmd>call VSCodeNotify("workbench.action.moveEditorToRightGroup")<cr>')

	map("n", "<leader>wr", '<cmd>call VSCodeNotify("workbench.action.increaseViewSize")<cr>')
	map("n", "<leader>wR", '<cmd>call VSCodeNotify("workbench.action.decreaseViewSize")<cr>')
else
	map("n", "<leader>wq", "<C-W>q", { desc = "Close split" })
	map("n", "<leader>ww", "<cmd>WindowsMaximizeHorizontally<cr>", { desc = "Maximize window" })

	map("n", "<leader>wh", "<cmd>lua require('utils').move_create_split('h')<cr>", { desc = "Focus left" })
	map("n", "<leader>wj", "<cmd>lua require('utils').move_create_split('j')<cr>", { desc = "Focus down" })
	map("n", "<leader>wk", "<cmd>lua require('utils').move_create_split('k')<cr>", { desc = "Focus up" })
	map("n", "<leader>wl", "<cmd>lua require('utils').move_create_split('l')<cr>", { desc = "Focus right" })

	map("n", "<leader>wH", "<C-W>H", { desc = "Move left" })
	map("n", "<leader>wJ", "<C-W>J", { desc = "Move down" })
	map("n", "<leader>wK", "<C-W>K", { desc = "Move up" })
	map("n", "<leader>wL", "<C-W>L", { desc = "Move right" })

	map("n", "<leader>w<up>", ":resize +15<cr>") -- FIXME: fix the direction on these
	map("n", "<leader>w<down>", ":resize -15<cr>")
	map("n", "<leader>w<left>", ":vertical resize -15<cr>")
	map("n", "<leader>w<right>", ":vertical resize +15<cr>")
end

-- Buffers
if vscode then
	map("n", "<leader>bh", '<cmd>call VSCodeNotify("workbench.action.previousEditor")<cr>')
	map("n", "<leader>bl", '<cmd>call VSCodeNotify("workbench.action.nextEditor")<cr>')
	map("n", "<leader>bq", '<cmd>call VSCodeNotify("workbench.action.closeActiveEditor")<cr>')
	map("n", "<leader>bp", '<cmd>call VSCodeNotify("workbench.action.pinEditor")<cr>')
else
	map("n", "<leader>bb", "<cmd>BufferPick<cr>", { desc = "Pick" })
	map("n", "<leader>bq", "<cmd>BufferClose<cr>", { desc = "Close" })
	map("n", "<leader>bQ", "<cmd>BufferCloseAllButCurrentOrPinned<cr>", { desc = "Close all" })
	map("n", "<leader>bp", "<cmd>BufferPin<cr>", { desc = "Pin" })
	map("n", "<leader>bf", "<cmd>lua require('telescope.builtin').buffers()<cr>", { desc = "Find" })
	map("n", "<leader>bl", "<cmd>BufferNext<cr>", { desc = "Next" })
	map("n", "<leader>bh", "<cmd>BufferPrevious<cr>", { desc = "Prev" })
	map("n", "<leader>bL", "<cmd>BufferMoveNext<cr>", { desc = "Move Next" })
	map("n", "<leader>bH", "<cmd>BufferMovePrevious<cr>", { desc = "Move Prev" })
end

-- Find
if vscode then
	map("n", "<leader>ff", '<cmd>call VSCodeNotify("workbench.action.findInFiles")<cr>')
	map("n", "<leader>fr", '<cmd>call VSCodeNotify("workbench.action.openRecent")<cr>')
	map("n", "<leader>fs", '<cmd>call VSCodeNotify("editor.action.selectHighlights")<cr>')
	map("n", "<leader>fn", '<cmd>call VSCodeNotify("workbench.files.action.showActiveFileInExplorer")<cr>')
	map("n", "<leader>fp", '<cmd>call VSCodeNotify("projectManager.listProjects")<cr>')
else
	map("n", "<leader>ff", function()
		require("telescope.builtin").find_files()
	end, { desc = "Files" })
	map("n", "<leader>fg", "<cmd>lua require('utils').live_grep_from_project_git_root()<cr>", { desc = "Grep" })
	map("n", "<leader>fr", function()
		require("telescope.builtin").oldfiles()
	end, { desc = "Recent" })
	map("n", "<leader>fs", function()
		require("telescope.builtin").grep_string()
	end, { desc = "String" })
	map("n", "<leader>fn", "<cmd>Neotree reveal_force_cwd=true toggle=true<cr>", { desc = "Tree" })
	map("n", "<leader>fp", function()
		require("telescope").extensions.project.project()
	end, { desc = "Project" })
	map("n", "<leader>ft", "<cmd>TodoTelescope<cr>", { desc = "Todo list" })
end

-- Leap
map({ "n", "x", "o" }, "s", "<Plug>(leap-forward-to)", { desc = "Leap forward" })
map({ "n", "x", "o" }, "S", "<Plug>(leap-backward-to)", { desc = "Leap backward" })
map({ "n", "x", "o" }, "<leader>s", "<Plug>(leap-from-window)", { desc = "Leap window" })

--Go to
if vscode then
else
	function map_lsp(bufnr)
		map("n", "<leader>gD", "<cmd>lua vim.lsp.buf.declaration()<cr>", { desc = "Declaration", buffer = bufnr })
		map("n", "<leader>gd", "<cmd>lua vim.lsp.buf.definition()<cr>", { desc = "Definition", buffer = bufnr })
		map(
			"n",
			"<leader>gt",
			"<cmd>lua vim.lsp.buf.type_definition()<cr>",
			{ desc = "Type Definition", buffer = bufnr }
		)
		map(
			"n",
			"<leader>gr",
			"<cmd>lua vim.lsp.buf.references()<cr>",
			{ desc = "Find all references", buffer = bufnr }
		)
		map("n", "<leader>gi", "<cmd>lua vim.lsp.buf.implementation()<cr>", { desc = "Implementation", buffer = bufnr })
		map("n", "<leader>gs", "<cmd>lua vim.lsp.buf.signature_help()<cr>", { desc = "Signature", buffer = bufnr })
		map("n", "<leader>gh", "<cmd>lua vim.lsp.buf.hover()<cr>", { desc = "Hover", buffer = bufnr })
		map("n", "<leader>ga", "<cmd>lua vim.lsp.buf.code_action()<cr>", { desc = "Code Action", buffer = bufnr })
		map("n", "<leader>ge", "<cmd>lua vim.diagnostic.open_float()<cr>", { desc = "Show Error", buffer = bufnr })
		map("n", "<leader>gE", "<cmd>TroubleToggle<cr>", { desc = "Error List", buffer = bufnr })

		map("n", "[e", "<cmd>lua vim.diagnostic.goto_prev()<cr>", { desc = "Previous error", buffer = bufnr })
		map("n", "]e", "<cmd>lua vim.diagnostic.goto_next()<cr>", { desc = "Next error", buffer = bufnr })
	end
end

-- Code
if vscode then
	map("n", "<leader>co", '<cmd>call VSCodeNotify("outline.focus")<cr>')
else
	map("n", "<Leader>cd", ":lua require('neogen').generate()<CR>", { desc = "Generate function" })
	map("n", "<leader>cf", "<cmd>NullFormat<cr>", { desc = "Format with null-ls" })
	map("n", "<leader>co", "<cmd>AerialToggle!<cr>", { desc = "Aerial Outline" })
end

-- Refactoring
map(
	"v",
	"<leader>rf",
	[[ <Esc><cmd>lua require('refactoring').refactor('Extract Function')<cr>]],
	{ desc = "Extract to a function", expr = false }
)
map(
	"v",
	"<leader>rv",
	[[ <Esc><cmd>lua require('refactoring').refactor('Extract Variable')<cr>]],
	{ desc = "Extract to a variable", expr = false }
)
map(
	"n",
	"<leader>ri",
	[[ <cmd>lua require('refactoring').refactor('Inline Variable')<cr>]],
	{ desc = "Inline a variable", expr = false }
)
map("n", "<leader>rr", function()
	return ":IncRename " .. vim.fn.expand("<cword>")
end, { desc = "Rename", expr = true })
map(
	"n",
	"<leader>rpf",
	":lua require('refactoring').debug.printf({below = false})<cr>",
	{ desc = "Add print statement (function)" }
)
map(
	"n",
	"<leader>rpv",
	":lua require('refactoring').debug.print_var({ normal = true })<cr>",
	{ desc = "Add print statement (variable)" }
)
map("n", "<leader>rpc", ":lua require('refactoring').debug.cleanup({})<cr>", { desc = "Cleanup print statements" })

-- Snippets
if not vscode then
	map({ "i", "s" }, "<c-j>", function()
		if require("luasnip").expand_or_jumpable() then
			require("luasnip").expand_or_jump()
		end
	end, { desc = "Next snippet placeholder" })

	map({ "i", "s" }, "<c-k>", function()
		if require("luasnip").jumpable(-1) then
			require("luasnip").jump(-1)
		end
	end, { desc = "Previous snippet placeholder" })

	map("i", "<c-h>", "<cmd>lua require('luasnip.extras.select_choice')()<cr>", { desc = "Select choice" })
end

-- Yanky
if not vscode then
	map({ "n", "x" }, "<leader>y", "<cmd>Telescope yank_history<cr>", { desc = "Yanks " })
	map({ "n", "x" }, "y", "<Plug>(YankyYank)")
	map({ "n", "x" }, "p", "<Plug>(YankyPutAfter)")
	map({ "n", "x" }, "P", "<Plug>(YankyPutBefore)")
	map("n", "<c-n>", "<Plug>(YankyCycleForward)")
	map("n", "<c-p>", "<Plug>(YankyCycleBackward)")
end

-- Terminal
if vscode then
	map("n", "<leader>t", '<cmd>call VSCodeNotify("workbench.action.terminal.toggleTerminal")<cr>')
else
	map("n", "<leader>t", "<cmd>ToggleTerm<cr>", { desc = "Terminal" })
	map("t", "<esc>", "<C-\\><C-n>") -- Escape to normal mode in terminal
end

-- Git
if vscode then
	map("n", "<leader>G", '<cmd>call VSCodeNotify("workbench.view.scm")<cr>')
else
	map("n", "<leader>G", "<cmd>lua	require('utils').toggle_lazygit()<cr>", { desc = "Git" })
end

-- todo-comments
if not vscode then
	map("n", "]t", function()
		require("todo-comments").jump_next()
	end, { desc = "Next todo comment" })

	map("n", "[t", function()
		require("todo-comments").jump_prev()
	end, { desc = "Previous todo comment" })
end

-- Jupynium
if not vscode then
	local function insert_above(code)
		vim.api.nvim_command("lua require('jupynium.textobj').goto_current_cell_separator()")
		vim.cmd("call append(line('.')-1, '')")
		vim.cmd("call append(line('.')-1, '')")
		vim.cmd("call append(line('.')-1, '')")
		vim.api.nvim_buf_set_lines(0, vim.fn.line(".") - 4, vim.fn.line(".") - 3, false, { code })
		vim.api.nvim_win_set_cursor(0, { vim.fn.line(".") - 2, 0 })
		vim.cmd("startinsert")
	end

	local function insert_closing_tag(code)
		vim.cmd("call append(line('.')-1, '')")
		vim.api.nvim_buf_set_lines(0, vim.fn.line(".") - 1, vim.fn.line("."), false, { code })
		vim.api.nvim_win_set_cursor(0, { vim.fn.line(".") - 1, 0 })
	end

	local cell_code = "# %%"
	local cell_md_open = '"""%%'
	local cell_md_close = '%%"""'

	map({ "n", "x" }, "<leader>jS", "<cmd>JupyniumStartAndAttachToServer<cr>", { desc = "Start Jupynium server" })
	map({ "n", "x" }, "<leader>js", "<cmd>JupyniumStartSync<cr>", { desc = "Sync Jupynium" })
	map({ "n", "x" }, "<leader>jh", "<cmd>JupyniumKernelHover<cr>", { desc = "Hover" })
	map({ "n", "x" }, "<leader>jkr", "<cmd>JupyniumKernelRestart<cr>", { desc = "Restart kernel" })
	map({ "n", "x" }, "<leader>jks", "<cmd>JupyniumKernelSelect<cr>", { desc = "Select kernel" })
	map({ "n", "x" }, "<leader>jki", "<cmd>JupyniumKernelInterrupt<cr>", { desc = "Interrupt kernel" })

	map({ "n", "x" }, "<leader>jac", function()
		insert_above(cell_code)
	end, { desc = "Insert code cell above" })
	map({ "n", "x" }, "<leader>jam", function()
		insert_above(cell_md_open)
		insert_closing_tag(cell_md_close)
	end, { desc = "Insert markdown cell above" })

	map(
		{ "n", "x", "o" },
		"<leader>jj",
		"<cmd>lua require'jupynium.textobj'.goto_current_cell_separator()<cr>",
		{ desc = "Go to current cell" }
	)
	map({ "n", "x" }, "<leader>je", "<cmd>JupyniumExecuteSelectedCells<cr>", { desc = "Execute cell" })
	map({ "n", "x" }, "<leader>jE", "ggVG<cmd>JupyniumExecuteSelectedCells<cr><esc>", { desc = "Execute all cells" }) -- TODO: jump back to prev cursor location
	map({ "n", "x" }, "<leader>joc", "<cmd>JupyniumClearSelectedCellsOutputs<cr>", { desc = "Clear output" })
	map({ "n", "x" }, "<leader>jot", "<cmd>JupyniumToggleSelectedCellsOutputsScroll<cr>", { desc = "Toggle output" })

	-- text objects
	map(
		{ "n", "x", "o" },
		"[j",
		"<cmd>lua require'jupynium.textobj'.goto_previous_cell_separator()<cr>",
		{ desc = "Previous jupyter cell" }
	)
	map(
		{ "n", "x", "o" },
		"]j",
		"<cmd>lua require'jupynium.textobj'.goto_next_cell_separator()<cr>",
		{ desc = "Next jupyter cell" }
	)
	map(
		{ "x", "o" },
		"aj",
		"<cmd>lua require'jupynium.textobj'.select_cell(true, false)<cr>",
		{ desc = "Around jupyter cell" }
	)
	map(
		{ "x", "o" },
		"ij",
		"<cmd>lua require'jupynium.textobj'.select_cell(false, false)<cr>",
		{ desc = "Inside jupyter cell" }
	)
end
