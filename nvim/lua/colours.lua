-- set default Todo HL to same as Comment (plugins can use their own HL group for Todo:)
vim.api.nvim_set_hl(0, "Todo", { link = "Comment" })
vim.api.nvim_set_hl(0, "hl_incline", { bg = "white", fg = "black" })

local C = {}
C.normal = "#af88dd"
C.insert = require("rose-pine.palette").gold
C.visual = require("rose-pine.palette").foam
C.command = require("rose-pine.palette").rose
C.replace = require("rose-pine.palette").love
C.text = require("rose-pine.palette").text
C.comment = require("rose-pine.palette").muted
C.overlay = require("rose-pine.palette").overlay
C.sep = require("rose-pine.palette").subtle
return C
