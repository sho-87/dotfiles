local M = {
	"L3MON4D3/LuaSnip",
	cond = vim.g.vscode == nil,
	version = "1.*",
	enabled = true,
	dependencies = "benfowler/telescope-luasnip.nvim",
	event = { "InsertEnter", "ModeChanged" },
}

function M.config()
	require("luasnip").setup({
		store_selection_keys = "<Tab>", -- Use <Tab> to trigger visual selection
		history = true,
		update_events = "TextChanged, TextChangedI",
		enable_autosnippets = true,
	})
	require("luasnip.loaders.from_lua").lazy_load({ paths = vim.fn.stdpath("config") .. "/luasnippets" })
	require("telescope").load_extension("luasnip")
end

return M