local M = {
	"lukas-reineke/headlines.nvim",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = "nvim-treesitter/nvim-treesitter",
	ft = { "markdown", "rmd", "norg" },
}

function M.config()
	require("headlines").setup({
		markdown = {
			headline_highlights = { "Headline1", "Headline2" },
			codeblock_highlight = "CodeBlock",
			dash_highlight = "Dash",
			dash_string = "-",
			quote_highlight = "Quote",
			quote_string = "┃",
			fat_headlines = true,
			fat_headline_upper_string = "▃",
			fat_headline_lower_string = "-",
		},
		rmd = {
			treesitter_language = "markdown",
			headline_highlights = { "Headline1", "Headline2" },
			codeblock_highlight = "CodeBlock",
			dash_highlight = "Dash",
			dash_string = "-",
			quote_highlight = "Quote",
			quote_string = "┃",
			fat_headlines = true,
			fat_headline_upper_string = "▃",
			fat_headline_lower_string = "-",
		},
		norg = {
			headline_highlights = { "Headline1", "Headline2" },
			codeblock_highlight = "CodeBlock",
			dash_highlight = "Dash",
			dash_string = "-",
			doubledash_highlight = "DoubleDash",
			doubledash_string = "=",
			quote_highlight = "Quote",
			quote_string = "┃",
			fat_headlines = true,
			fat_headline_upper_string = "▃",
			fat_headline_lower_string = "-",
		},
	})
end

return M