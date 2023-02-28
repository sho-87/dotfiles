local M = {
	"L3MON4D3/LuaSnip",
	cond = vim.g.vscode == nil,
	version = "1.*",
	dependencies = "rafamadriz/friendly-snippets",
	enabled = true,
	event = { "InsertEnter", "ModeChanged" },
}

function M.config()
	require("luasnip").config.set_config({
		store_selection_keys = "<Tab>", -- Use <Tab> to trigger visual selection
	})
	require("luasnip.loaders.from_vscode").lazy_load()
	require("luasnip.loaders.from_snipmate").lazy_load()
end

return M
