local M = {
	"ThePrimeagen/refactoring.nvim",
	enabled = true,
	dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-lua/plenary.nvim" },
	event = { "VeryLazy" },
}

function M.config()
	require("refactoring").setup()
end

return M
