local ls = require("luasnip")
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local l = require("luasnip.extras").lambda
local rep = require("luasnip.extras").rep
local p = require("luasnip.extras").partial
local m = require("luasnip.extras").match
local n = require("luasnip.extras").nonempty
local dl = require("luasnip.extras").dynamic_lambda
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local types = require("luasnip.util.types")
local conds = require("luasnip.extras.conditions")
local conds_expand = require("luasnip.extras.conditions.expand")

return {
	s(
		{ trig = "ifmain", dscr = "If name == main", snippetType = "autosnippet", regTrig = false },
		fmta(
			[[
            if __name__ == "__main__":
                <>
        ]],
			{ i(1,"pass") }
		)
	),
	s(
		{ trig = "frin", dscr = "For loop", snippetType = "autosnippet", regTrig = false },
		fmta(
			[[
            for <> in <>:
                <>
        ]],
			{
				f(function(args)
					s = args[1][1]
					return s:sub(1, 1):lower()
				end, { 1 }),
				i(1),
				i(0, "pass"),
			}
		)
	),
	s(
		{ trig = "fren", dscr = "For enumerate", snippetType = "autosnippet", regTrig = false },
		fmta(
			[[
            for i, <> in enumerate(<>):
                <>
        ]],
			{
				f(function(args)
					s = args[1][1]
					return s:sub(1, 1):lower()
				end, { 1 }),
				i(1),
				i(0, "pass"),
			}
		)
	),
    s(
		{ trig = "def", dscr = "Define function", snippetType = "autosnippet", regTrig = false },
		fmt(
			[[
            def {}({}: {}) -> {}:
                """
                {}

                Args:
                    {} ({}): {}

                Returns:
                    {}: {}

                Example:
                    >>> {}({})
                """
                {}
        ]],
			{
				i(1, "function"),
				i(2, "arg"),
				i(3, "type"),
				i(4, "return_type"),
				i(5, "docstring"),
				rep(2),
				rep(3),
				i(6),
				rep(4),
				i(7),
				rep(1),
				rep(2),
				i(0, "pass"),
			}
		)
	),
}
