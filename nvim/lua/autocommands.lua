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
