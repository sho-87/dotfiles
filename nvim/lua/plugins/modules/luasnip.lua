local M = {
	"L3MON4D3/LuaSnip",
	cond = vim.g.vscode == nil,
	version = "1.*",
	dependencies = "rafamadriz/friendly-snippets",
	enabled = true,
	event = "InsertEnter",
}

function M.config()
	require("luasnip.loaders.from_vscode").lazy_load()
end

return M
