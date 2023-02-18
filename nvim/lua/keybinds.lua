local map = vim.keymap.set
local vscode = vim.g.vscode
local noremap = {
	noremap = true,
	silent = true,
}

-- General
map("i", "jj", "<ESC>", noremap) -- Exit insert mode
map("n", "<ESC>", ":nohlsearch<CR>", noremap) -- Clear highlights on ESC
map("n", "cd", ":cd %:p:h<CR>:pwd<CR>", noremap) -- Change directory to current file's directory
map("n", "<leader>qq", ":qa<CR>", noremap) -- Quit all windows

-- Help
if vscode then
	map("n", "<leader>?k", '<Cmd>call VSCodeNotify("workbench.action.keybindingsReference")<CR>', noremap)
else
	map("n", "<leader>?", "{}", { desc = "Help" }) -- prefix
	map("n", "<leader>?k", require("telescope.builtin").keymaps, { desc = "Keymaps" })
end

-- Hop
map("n", "<leader>h", "{}", { desc = "Hop" }) -- prefix
map("n", "<leader>hh", ":HopChar2<CR>", { desc = "2 chars" })
map("n", "<leader>h/", ":HopPattern<CR>", { desc = "Pattern" })

-- Splits
if vscode then
	map("n", "<leader>wv", '<Cmd>call VSCodeNotify("workbench.action.splitEditorRight")<CR>', noremap)
	map("n", "<leader>ws", '<Cmd>call VSCodeNotify("workbench.action.splitEditorDown")<CR>', noremap)
	map("n", "<leader>wq", '<Cmd>call VSCodeNotify("workbench.action.closeActiveEditor")<CR>', noremap)

	map("n", "<leader>wh", '<Cmd>call VSCodeNotify("workbench.action.focusLeftGroup")<CR>', noremap)
	map("n", "<leader>wj", '<Cmd>call VSCodeNotify("workbench.action.focusBelowGroup")<CR>', noremap)
	map("n", "<leader>wk", '<Cmd>call VSCodeNotify("workbench.action.focusAboveGroup")<CR>', noremap)
	map("n", "<leader>wl", '<Cmd>call VSCodeNotify("workbench.action.focusRightGroup")<CR>', noremap)

	map("n", "<leader>wH", '<Cmd>call VSCodeNotify("workbench.action.moveEditorToLeftGroup")<CR>', noremap)
	map("n", "<leader>wJ", '<Cmd>call VSCodeNotify("workbench.action.moveEditorToBelowGroup")<CR>', noremap)
	map("n", "<leader>wK", '<Cmd>call VSCodeNotify("workbench.action.moveEditorToAboveGroup")<CR>', noremap)
	map("n", "<leader>wL", '<Cmd>call VSCodeNotify("workbench.action.moveEditorToRightGroup")<CR>', noremap)

	map("n", "<leader>wr", '<Cmd>call VSCodeNotify("workbench.action.increaseViewSize")<CR>', noremap)
	map("n", "<leader>wR", '<Cmd>call VSCodeNotify("workbench.action.decreaseViewSize")<CR>', noremap)
else
	map("n", "<leader>w", "{}", { desc = "Window" }) -- prefix
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

-- Find
if vscode then
	map("n", "<leader>ff", '<Cmd>call VSCodeNotify("workbench.action.findInFiles")<CR>', noremap)
	map("n", "<leader>fr", '<Cmd>call VSCodeNotify("workbench.action.openRecent")<CR>', noremap)
	map("n", "<leader>fs", '<Cmd>call VSCodeNotify("editor.action.selectHighlights")<CR>', noremap)
	map("n", "<leader>fn", '<Cmd>call VSCodeNotify("workbench.files.action.showActiveFileInExplorer")<CR>', noremap)
	map("n", "<leader>fp", '<Cmd>call VSCodeNotify("projectManager.listProjects")<CR>', noremap)
else
	map("n", "<leader>f", "{}", { desc = "Find" }) -- prefix
	map("n", "<leader>ff", require("telescope.builtin").find_files, { desc = "Files" })
	map("n", "<leader>fg", require("telescope.builtin").live_grep, { desc = "Grep" })
	map("n", "<leader>fr", require("telescope.builtin").oldfiles, { desc = "Recent" })
	map("n", "<leader>fs", require("telescope.builtin").grep_string, { desc = "String" })
	map("n", "<leader>fn", ":Neotree reveal_force_cwd=true toggle=true<CR>", { desc = "Tree" })
	map("n", "<leader>fp", require("telescope").extensions.project.project, { desc = "Project" })
end

-- LSP
if vscode then
	map("n", "<leader>go", '<Cmd>call VSCodeNotify("outline.focus")<CR>', noremap)
end

-- Folds
-- if vscode then
-- 	map("n", "za", '<Cmd>call VSCodeNotify("editor.toggleFold")<CR>', noremap)
-- 	map("n", "zo", '<Cmd>call VSCodeNotify("editor.unfoldAll")<CR>', noremap)
-- 	map("n", "zc", '<Cmd>call VSCodeNotify("editor.foldAll")<CR>', noremap)
-- else
-- 	map("n", "z", "{}", { desc = "Folds" }) -- prefix
-- 	map("n", "za", "za", { desc = "Toggle" })
-- 	map("n", "zo", require("ufo").openAllFolds, { desc = "Open all" })
-- 	map("n", "zc", require("ufo").closeAllFolds, { desc = "Close all" })
-- end

-- Yanky
if vscode then
	map("n", "p", '<Cmd>call VSCodeNotify("editor.action.clipboardPasteAction")<CR>', noremap)
	map("n", "P", '<Cmd>call VSCodeNotify("editor.action.clipboardPasteAction")<CR>', noremap)
else
	map({ "n", "x" }, "<c-y>", ":Telescope yank_history<CR>", noremap)
	map({ "n", "x" }, "y", "<Plug>(YankyYank)", noremap)
	map({ "n", "x" }, "p", "<Plug>(YankyPutAfter)", noremap)
	map({ "n", "x" }, "P", "<Plug>(YankyPutBefore)", noremap)
	map("n", "<c-n>", "<Plug>(YankyCycleForward)", noremap)
	map("n", "<c-p>", "<Plug>(YankyCycleBackward)", noremap)
end

-- Buffers
if vscode then
	map("n", "<leader>bh", '<Cmd>call VSCodeNotify("workbench.action.previousEditor")<CR>', noremap)
	map("n", "<leader>bl", '<Cmd>call VSCodeNotify("workbench.action.nextEditor")<CR>', noremap)
	map("n", "<leader>bx", '<Cmd>call VSCodeNotify("workbench.action.closeActiveEditor")<CR>', noremap)
	map("n", "<leader>bp", '<Cmd>call VSCodeNotify("workbench.action.pinEditor")<CR>', noremap)
else
	map("n", "<leader>b", "{}", { desc = "Buffer" }) -- prefix
	map("n", "<leader>bb", "<Cmd>BufferPick<CR>", { desc = "Pick" })
	map("n", "<leader>bx", "<Cmd>BufferClose<CR>", { desc = "Close" })
	map("n", "<leader>bp", "<Cmd>BufferPin<CR>", { desc = "Pin" })
	map("n", "<leader>bf", require("telescope.builtin").buffers, { desc = "Find" })
	map("n", "<leader>bl", "<Cmd>BufferNext<CR>", { desc = "Next" })
	map("n", "<leader>bh", "<Cmd>BufferPrevious<CR>", { desc = "Prev" })
	map("n", "<leader>bL", "<Cmd>BufferMoveNext<CR>", { desc = "Move Next" })
	map("n", "<leader>bH", "<Cmd>BufferMovePrevious<CR>", { desc = "Move Prev" })
end

-- Minimap
-- if vscode then
-- 	map("n", "<leader>mm", '<Cmd>call VSCodeNotify("editor.action.toggleMinimap")<CR>', noremap)
-- else
-- 	map("n", "<Leader>m", "{}", { desc = "Minimap" }) -- prefix
-- 	map("n", "<Leader>mf", require("mini.map").toggle_focus, { desc = "Focus" })
-- 	map("n", "<Leader>mr", require("mini.map").refresh, { desc = "Refresh" })
-- 	map("n", "<Leader>mm", require("mini.map").toggle, { desc = "Toggle" })
-- end
