local map = require("utils").map
local vscode = vim.g.vscode

-- General
map("i", "jj", "<ESC>") -- Exit insert mode
map("n", "<ESC>", "<Cmd>nohlsearch<CR>", { desc = "Clear highlights" }) -- Clear highlights on ESC
map("n", "cd", ":cd %:p:h<CR>:pwd<CR>", { desc = "Change working directory" }) -- Change directory to current file's directory
map("n", "<leader>qq", "<Cmd>qa<CR>") -- Quit all windows

-- Undo (rest are in telescope module)
map("n", "<leader>u", "<Cmd>Telescope undo<CR>", { desc = "Undo tree" })

-- Help
if vscode then
	map("n", "<leader>?k", '<Cmd>call VSCodeNotify("workbench.action.keybindingsReference")<CR>')
else
	map("n", "<leader>?", "{}", { desc = "Help" })
	map("n", "<leader>?h", require("telescope.builtin").help_tags, { desc = "Help" })
	map("n", "<leader>?k", require("telescope.builtin").keymaps, { desc = "Keymaps" })
	map("n", "<leader>?c", require("telescope.builtin").commands, { desc = "Commands" })
	map("n", "<leader>?a", require("telescope.builtin").autocommands, { desc = "Autocommands" })
	map("n", "<leader>?g", require("telescope.builtin").highlights, { desc = "Highlight groups" })
	map("n", "<leader>?v", require("telescope.builtin").vim_options, { desc = "Vim options" })
	map("n", "<leader>?n", "<Cmd>Telescope notify<CR>", { desc = "Notifications" })
end

-- Splits
if vscode then
	map("n", "<leader>wv", '<Cmd>call VSCodeNotify("workbench.action.splitEditorRight")<CR>')
	map("n", "<leader>ws", '<Cmd>call VSCodeNotify("workbench.action.splitEditorDown")<CR>')
	map("n", "<leader>wq", '<Cmd>call VSCodeNotify("workbench.action.closeActiveEditor")<CR>')

	map("n", "<leader>wh", '<Cmd>call VSCodeNotify("workbench.action.focusLeftGroup")<CR>')
	map("n", "<leader>wj", '<Cmd>call VSCodeNotify("workbench.action.focusBelowGroup")<CR>')
	map("n", "<leader>wk", '<Cmd>call VSCodeNotify("workbench.action.focusAboveGroup")<CR>')
	map("n", "<leader>wl", '<Cmd>call VSCodeNotify("workbench.action.focusRightGroup")<CR>')

	map("n", "<leader>wH", '<Cmd>call VSCodeNotify("workbench.action.moveEditorToLeftGroup")<CR>')
	map("n", "<leader>wJ", '<Cmd>call VSCodeNotify("workbench.action.moveEditorToBelowGroup")<CR>')
	map("n", "<leader>wK", '<Cmd>call VSCodeNotify("workbench.action.moveEditorToAboveGroup")<CR>')
	map("n", "<leader>wL", '<Cmd>call VSCodeNotify("workbench.action.moveEditorToRightGroup")<CR>')

	map("n", "<leader>wr", '<Cmd>call VSCodeNotify("workbench.action.increaseViewSize")<CR>')
	map("n", "<leader>wR", '<Cmd>call VSCodeNotify("workbench.action.decreaseViewSize")<CR>')
else
	map("n", "<leader>wv", "<C-W>v", { desc = "Split: vertical" })
	map("n", "<leader>ws", "<C-W>s", { desc = "Split: horizontal" })
	map("n", "<leader>wq", "<C-W>q", { desc = "Split: close" })

	map("n", "<leader>wh", require("smart-splits").move_cursor_left, { desc = "Focus: left" })
	map("n", "<leader>wj", require("smart-splits").move_cursor_down, { desc = "Focus: down" })
	map("n", "<leader>wk", require("smart-splits").move_cursor_up, { desc = "Focus: up" })
	map("n", "<leader>wl", require("smart-splits").move_cursor_right, { desc = "Focus: right" })

	map("n", "<leader>wH", "<C-W>H", { desc = "Move: left" })
	map("n", "<leader>wJ", "<C-W>J", { desc = "Move: down" })
	map("n", "<leader>wK", "<C-W>K", { desc = "Move: up" })
	map("n", "<leader>wL", "<C-W>L", { desc = "Move: right" })

	map("n", "<leader>wr", require("smart-splits").start_resize_mode, { desc = "Resize mode" })
end

-- Buffers
if vscode then
	map("n", "<leader>bh", '<Cmd>call VSCodeNotify("workbench.action.previousEditor")<CR>')
	map("n", "<leader>bl", '<Cmd>call VSCodeNotify("workbench.action.nextEditor")<CR>')
	map("n", "<leader>bc", '<Cmd>call VSCodeNotify("workbench.action.closeActiveEditor")<CR>')
	map("n", "<leader>bp", '<Cmd>call VSCodeNotify("workbench.action.pinEditor")<CR>')
else
	map("n", "<leader>bb", "<Cmd>BufferPick<CR>", { desc = "Pick" })
	map("n", "<leader>bc", "<Cmd>BufferClose<CR>", { desc = "Close" })
	map("n", "<leader>bx", "<Cmd>BufferCloseAllButCurrentOrPinned<CR>", { desc = "Close all" })
	map("n", "<leader>bp", "<Cmd>BufferPin<CR>", { desc = "Pin" })
	map("n", "<leader>bf", require("telescope.builtin").buffers, { desc = "Find" })
	map("n", "<leader>bl", "<Cmd>BufferNext<CR>", { desc = "Next" })
	map("n", "<leader>bh", "<Cmd>BufferPrevious<CR>", { desc = "Prev" })
	map("n", "<leader>bL", "<Cmd>BufferMoveNext<CR>", { desc = "Move Next" })
	map("n", "<leader>bH", "<Cmd>BufferMovePrevious<CR>", { desc = "Move Prev" })
end

-- Find
if vscode then
	map("n", "<leader>ff", '<Cmd>call VSCodeNotify("workbench.action.findInFiles")<CR>')
	map("n", "<leader>fr", '<Cmd>call VSCodeNotify("workbench.action.openRecent")<CR>')
	map("n", "<leader>fs", '<Cmd>call VSCodeNotify("editor.action.selectHighlights")<CR>')
	map("n", "<leader>fn", '<Cmd>call VSCodeNotify("workbench.files.action.showActiveFileInExplorer")<CR>')
	map("n", "<leader>fp", '<Cmd>call VSCodeNotify("projectManager.listProjects")<CR>')
else
	map("n", "<leader>ff", require("telescope.builtin").find_files, { desc = "Files" })
	map("n", "<leader>fg", require("telescope.builtin").live_grep, { desc = "Grep" })
	map("n", "<leader>fr", require("telescope.builtin").oldfiles, { desc = "Recent" })
	map("n", "<leader>fs", require("telescope.builtin").grep_string, { desc = "String" })
	map("n", "<leader>fn", "<Cmd>Neotree reveal_force_cwd=true toggle=true<CR>", { desc = "Tree" })
	map("n", "<leader>fp", require("telescope").extensions.project.project, { desc = "Project" })
end

-- LSP
if vscode then
	map("n", "<leader>co", '<Cmd>call VSCodeNotify("outline.focus")<CR>')
else
	function map_lsp(client, bufnr)
		map("n", "<leader>cD", "<cmd>lua vim.lsp.buf.declaration()<cr>", { desc = "Declaration", buffer = bufnr })
		map("n", "<leader>cd", "<cmd>lua vim.lsp.buf.definition()<cr>", { desc = "Definition", buffer = bufnr })
		map(
			"n",
			"<leader>ct",
			"<cmd>lua vim.lsp.buf.type_definition()<cr>",
			{ desc = "Type Definition", buffer = bufnr }
		)
		map(
			"n",
			"<leader>cr",
			"<cmd>lua vim.lsp.buf.references()<cr>",
			{ desc = "Find all references", buffer = bufnr }
		)
		map("n", "<leader>ci", "<cmd>lua vim.lsp.buf.implementation()<cr>", { desc = "Implementation", buffer = bufnr })
		map("n", "<leader>cf", "<cmd>NullFormat<cr>", { desc = "Format with null-ls", buffer = bufnr })
		map("n", "<leader>cs", "<cmd>lua vim.lsp.buf.signature_help()<cr>", { desc = "Signature", buffer = bufnr })
		map("n", "<leader>ch", "<cmd>lua vim.lsp.buf.hover()<cr>", { desc = "Hover", buffer = bufnr })
		map("n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<cr>", { desc = "Code Action", buffer = bufnr })
		map("n", "<leader>co", require("telescope.builtin").treesitter, { desc = "Outline" })
		map("n", "<leader>ce", "<cmd>lua vim.diagnostic.open_float()<cr>", { desc = "Show Error", buffer = bufnr })
		map("n", "<leader>cE", "<cmd>TroubleToggle<cr>", { desc = "Error List", buffer = bufnr })

		map("n", "[e", "<cmd>lua vim.diagnostic.goto_prev()<cr>", { desc = "Previous error", buffer = bufnr })
		map("n", "]e", "<cmd>lua vim.diagnostic.goto_next()<cr>", { desc = "Next error", buffer = bufnr })
		map("n", "<leader>rr", "<cmd>lua vim.lsp.buf.rename()<cr>", { desc = "Rename", buffer = bufnr }) -- include in refactoring menu
	end
end

-- Refactoring
map(
	"v",
	"<leader>rf",
	[[ <Esc><Cmd>lua require('refactoring').refactor('Extract Function')<CR>]],
	{ desc = "Extract to a function", expr = false }
)
map(
	"v",
	"<leader>rv",
	[[ <Esc><Cmd>lua require('refactoring').refactor('Extract Variable')<CR>]],
	{ desc = "Extract to a variable", expr = false }
)
map(
	"n",
	"<leader>ri",
	[[ <Cmd>lua require('refactoring').refactor('Inline Variable')<CR>]],
	{ desc = "Inline a variable", expr = false }
)

map(
	"n",
	"<leader>rpf",
	":lua require('refactoring').debug.printf({below = false})<CR>",
	{ desc = "Add print statement (function)" }
)
map(
	"n",
	"<leader>rpv",
	":lua require('refactoring').debug.print_var({ normal = true })<CR>",
	{ desc = "Add print statement (variable)" }
)
map("n", "<leader>rpc", ":lua require('refactoring').debug.cleanup({})<CR>", { desc = "Cleanup print statements" })

-- Folds
if vscode then
	map("n", "za", '<Cmd>call VSCodeNotify("editor.toggleFold")<CR>')
	map("n", "zo", '<Cmd>call VSCodeNotify("editor.unfoldAll")<CR>')
	map("n", "zc", '<Cmd>call VSCodeNotify("editor.foldAll")<CR>')
else
	function map_ufo()
		map("n", "za", "za", { desc = "Toggle" })
		map("n", "zo", require("ufo").openAllFolds, { desc = "Open all" })
		map("n", "zc", require("ufo").closeAllFolds, { desc = "Close all" })
	end
end

-- Yanky
if vscode then
else
	map({ "n", "x" }, "<leader>y", "<Cmd>Telescope yank_history<CR>", { desc = "Yanks " })
	map({ "n", "x" }, "y", "<Plug>(YankyYank)")
	map({ "n", "x" }, "p", "<Plug>(YankyPutAfter)")
	map({ "n", "x" }, "P", "<Plug>(YankyPutBefore)")
	map("n", "<c-n>", "<Plug>(YankyCycleForward)")
	map("n", "<c-p>", "<Plug>(YankyCycleBackward)")
end

-- Terminal
if vscode then
	map("n", "<leader>t", '<Cmd>call VSCodeNotify("workbench.action.terminal.toggleTerminal")<CR>')
else
	map("n", "<leader>t", "<Cmd>ToggleTerm<CR>", { desc = "Terminal" })
	map("t", "<ESC>", "<C-\\><C-n>") -- Escap to normal mode in terminal
end

-- Git
if vscode then
	map("n", "<leader>g", '<Cmd>call VSCodeNotify("workbench.view.scm")<CR>')
else
	map("n", "<leader>g", require('utils').toggle_lazygit, { desc = "Git" })
end
