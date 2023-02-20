local M = {
	"nvim-treesitter/nvim-treesitter-context",
	cond = vim.g.vscode == nil,
	enabled = true,
	dependencies = "nvim-treesitter/nvim-treesitter",
	event = { "BufReadPost", "BufNewFile" },
}

function M.config()
	require("treesitter-context").setup({
		max_lines = 1, -- How many lines the window should span. Values <= 0 mean no limit.
		trim_scope = "outer", -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
		min_window_height = 2, -- Minimum editor window height to enable context. Values <= 0 mean no limit.
		patterns = { -- Match patterns for TS nodes. These get wrapped to match at word boundaries.
			-- For all filetypes
			-- Note that setting an entry here replaces all other patterns for this entry.
			-- By setting the 'default' entry below, you can control which nodes you want to
			-- appear in the context window.
			default = {
				"class",
				"function",
				"method",
				"for",
				"while",
				"if",
				"switch",
				"case",
				"interface",
				"struct",
				"enum",
			},
			-- Patterns for specific filetypes
			tex = {
				"chapter",
				"section",
				"subsection",
				"subsubsection",
			},
			haskell = {
				"adt",
			},
			rust = {
				"impl_item",
			},
			terraform = {
				"block",
				"object_elem",
				"attribute",
			},
			scala = {
				"object_definition",
			},
			vhdl = {
				"process_statement",
				"architecture_body",
				"entity_declaration",
			},
			markdown = {
				"section",
			},
			elixir = {
				"anonymous_function",
				"arguments",
				"block",
				"do_block",
				"list",
				"map",
				"tuple",
				"quoted_content",
			},
			json = {
				"pair",
			},
			typescript = {
				"export_statement",
			},
			yaml = {
				"block_mapping_pair",
			},
		},
		zindex = 20, -- The Z-index of the context window
		mode = "topline", -- Line used to calculate context. Choices: 'cursor', 'topline'
		-- Separator between context and content. Should be a single character string, like '-'.
		-- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
		separator = nil,
	})
end

return M
