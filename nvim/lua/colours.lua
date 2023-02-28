vim.cmd("highlight hl_incline guibg=white guifg=black")

local C = {}
C.normal = require("rose-pine.palette").iris
C.insert = require("rose-pine.palette").gold
C.visual = require("rose-pine.palette").foam
C.command = require("rose-pine.palette").rose
C.replace = require("rose-pine.palette").love
C.text = require("rose-pine.palette").text
C.comment = require("rose-pine.palette").muted
C.overlay = require("rose-pine.palette").overlay
C.sep = require("rose-pine.palette").subtle
return C
