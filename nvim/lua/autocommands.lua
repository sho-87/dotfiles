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

-- Stop newline comment continuation
vim.api.nvim_create_autocmd("BufEnter", {
	callback = function()
		vim.opt.formatoptions = vim.opt.formatoptions - { "c", "r", "o" }
	end,
})

-- Linenumbers depending on mode
-- local lineNumberGroup = vim.api.nvim_create_augroup("LineNumberMode", { clear = true })
-- vim.api.nvim_create_autocmd("InsertEnter", {
-- 	callback = function()
-- 		vim.opt.number = true
-- 		vim.opt.relativenumber = false
-- 	end,
-- 	group = lineNumberGroup,
-- })
-- vim.api.nvim_create_autocmd("InsertLeave", {
-- 	callback = function()
-- 		vim.opt.number = true
-- 		vim.opt.relativenumber = true
-- 	end,
-- 	group = lineNumberGroup,
-- })
-- vim.api.nvim_create_autocmd({ "WinEnter", "FocusGained" }, {
-- 	callback = function()
-- 		if (vim.bo.filetype ~= "neo-tree") and (vim.bo.filetype ~= "aerial") then
-- 			vim.opt.number = true
-- 			vim.opt.relativenumber = true
-- 		end
-- 	end,
-- 	group = lineNumberGroup,
-- })
-- vim.api.nvim_create_autocmd({ "WinLeave", "FocusLost" }, {
-- 	callback = function()
-- 		if (vim.bo.filetype ~= "neo-tree") and (vim.bo.filetype ~= "aerial") then
-- 			vim.opt.number = true
-- 			vim.opt.relativenumber = false
-- 		end
-- 	end,
-- 	group = lineNumberGroup,
-- })
