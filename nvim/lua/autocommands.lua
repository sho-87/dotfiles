-- Offset bufferline when file tree is open
local bufferlineGroup = vim.api.nvim_create_augroup("BufferlineOffset", { clear = true })
vim.api.nvim_create_autocmd("BufWinEnter", {
	callback = function(tbl)
		if vim.bo[tbl.buf].filetype == "neo-tree" then
			require("bufferline.api").set_offset(36, "FileTree")
		end
	end,
	group = bufferlineGroup,
})
vim.api.nvim_create_autocmd({ "BufWinLeave", "BufWipeout" }, {
	callback = function(tbl)
		if vim.bo[tbl.buf].filetype == "neo-tree" then
			require("bufferline.api").set_offset(0)
		end
	end,
	group = bufferlineGroup,
})

-- Handle terminal things
local terminalGroup = vim.api.nvim_create_augroup("TerminalSetup", { clear = true })
vim.api.nvim_create_autocmd("TermOpen", {
	command = "startinsert",
	group = terminalGroup,
})
vim.api.nvim_create_autocmd("TermEnter", {
	command = "setlocal nonu nornu signcolumn=no",
	group = terminalGroup,
})
vim.api.nvim_create_autocmd({ "BufEnter", "FocusGained", "WinEnter" }, {
	command = "if &nu | set rnu | endif",
	group = terminalGroup,
})
vim.api.nvim_create_autocmd({ "BufLeave", "FocusLost", "WinLeave" }, {
	command = "if &nu | set nornu | endif",
	group = terminalGroup,
})
