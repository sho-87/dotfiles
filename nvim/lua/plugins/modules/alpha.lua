local M = {
	"goolord/alpha-nvim",
	enabled = true,
	dependencies = { "nvim-tree/nvim-web-devicons" },
	event = "VimEnter",
}

function M.config()
	local theme = require("alpha.themes.theta")
	local config = theme.config

	-- Header
	local cool = {
		type = "text",
		val = {
			[[    ███╗   ███╗ █████╗ ██╗  ██╗███████╗   ]],
			[[    ████╗ ████║██╔══██╗██║ ██╔╝██╔════╝   ]],
			[[    ██╔████╔██║███████║█████╔╝ █████╗     ]],
			[[    ██║╚██╔╝██║██╔══██║██╔═██╗ ██╔══╝     ]],
			[[    ██║ ╚═╝ ██║██║  ██║██║  ██╗███████╗   ]],
			[[    ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝   ]],
			[[      ██████╗ ██████╗  ██████╗ ██╗        ]],
			[[     ██╔════╝██╔═══██╗██╔═══██╗██║        ]],
			[[     ██║     ██║   ██║██║   ██║██║        ]],
			[[     ██║     ██║   ██║██║   ██║██║        ]],
			[[     ╚██████╗╚██████╔╝╚██████╔╝███████╗   ]],
			[[      ╚═════╝ ╚═════╝  ╚═════╝ ╚══════╝   ]],
			[[███████╗████████╗██╗   ██╗███████╗███████╗]],
			[[██╔════╝╚══██╔══╝██║   ██║██╔════╝██╔════╝]],
			[[███████╗   ██║   ██║   ██║█████╗  █████╗  ]],
			[[╚════██║   ██║   ██║   ██║██╔══╝  ██╔══╝  ]],
			[[███████║   ██║   ╚██████╔╝██║     ██║     ]],
			[[╚══════╝   ╚═╝    ╚═════╝ ╚═╝     ╚═╝     ]],
		},
		opts = {
			position = "center",
			hl = "Boolean",
		},
	}

	local panda = {
		type = "text",
		val = {
			[[       ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⣤⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣠⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀        ]],
			[[       ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣾⣿⣿⣿⣿⣿⣿⣆⠀⢀⣀⣀⣤⣤⣤⣦⣦⣤⣤⣄⣀⣀⠀⢠⣾⣿⣿⣿⣿⣿⣷⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀    ]],
			[[       ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣿⣿⣿⣿⣿⣿⣿⣿⡿⠟⠛⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠙⠛⠿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀      ]],
			[[       ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⠟⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠙⢿⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀       ]],
			[[      ⠀ ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢻⣿⣿⣿⣿⡟⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⣿⣿⣿⣿⣿⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀       ]],
			[[      ⠀⠀ ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠻⢿⣿⠟⠀⠀⠀⠀⠀⣀⣤⣤⣤⡀⠀⠀⠀⠀⠀⢀⣤⣤⣤⣄⡀⠀⠀⠀⠀⠘⣿⡿⠿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀      ]],
			[[      ⠀⠀⠀ ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⡟⠀⠀⠀⠀⣠⣾⣿⣿⣟⣿⡇⠀⠀⠀⠀⠀⢸⣿⣿⣻⣿⣿⣦⠀⠀⠀⠀⠸⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀       ]],
			[[      ⠀⠀⠀⠀ ⠀⠀⠀⠀⠀⠀⠀⠀⠀⣼⠁⠀⠀⠀⠀⣿⣿⣿⣿⣿⡟⢠⣶⣾⣿⣿⣷⣤⢽⣿⣿⣿⣿⣿⡇⠀⠀⣀⣤⣿⣷⣴⣶⣦⣀⡀⠀⠀⠀⠀⠀⠀⠀     ]],
			[[      ⠀⠀⠀⠀⠀ ⠀⠀⠀⢀⣠⣤⣤⣠⣇⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⠀⠘⠻⣿⣿⣿⡿⠋⠀⢹⣿⣿⣿⣿⡇⠀⣿⣿⣿⡏⢹⣿⠉⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀    ]],
			[[      ⠀⠀⠀⠀ ⠀⠀⢠⣾⣿⣿⣿⣿⣿⣿⣿⣶⣄⠀⠀⠹⣿⣿⠿⠋⠀⢤⣀⢀⣼⡄⠀⣠⠀⠈⠻⣿⣿⠟⠀⢸⣿⣇⣽⣿⠿⠿⠿⣿⣅⣽⣿⡇⠀⠀⠀⠀⠀   ]],
			[[      ⠀⠀⠀⠀⠀ ⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣆⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠁⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠈⣿⣿⣟⠁⠀⠀⠀⠈⣿⣿⣿⡇⠀⠀⠀⠀      ]],
			[[       ⠛⠛⠛⠛⠛⠛⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛]],
			[[       ⠀⠀⠀⠀⠀⠀⠘⠛⠻⢿⣿⣿⣿⣿⣿⠟⠛⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀       ]],
			[[       ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠀⠈⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀         ]],
		},
		opts = {
			position = "center",
			hl = "Character",
		},
	}

	local function random_header(headers)
		math.randomseed(os.time())
		return headers[math.random(#headers)]
	end

	-- Links / buttons
	local dashboard = require("alpha.themes.dashboard")
	local links = {
		type = "group",
		val = {
			{ type = "text", val = "Tools", opts = { hl = "SpecialComment", position = "center" } },
			dashboard.button("l", "💤 Lazy", "<cmd>Lazy<CR>"),
			dashboard.button("m", "🧱 Mason", "<cmd>Mason<CR>"),
		},
		position = "center",
	}

	-- MRU
	local function get_mru(num)
		local tbl = {
			{ type = "text", val = "Recent Files", opts = { hl = "SpecialComment", position = "center" } },
		}

		local mru_list = theme.mru(1, "", num)

		for i, file in ipairs(mru_list.val) do
			table.insert(tbl, file)
		end

		return { type = "group", val = tbl, opts = {} }
	end

	-- Projects
	local function get_projects(num)
		local items_number = num
		local alphabet = "abcdefghijknopqrstuvwxyz"

		local project_list = require("telescope._extensions.project.utils").get_projects("recent")

		local tbl = {
			{ type = "text", val = "Projects", opts = { hl = "SpecialComment", position = "center" } },
		}

		for i, project in ipairs(project_list) do
			if i > items_number then
				break
			end
			local path_escape = project.path:gsub("\\", "\\\\")
			print(path_escape)
			local file_button_el = dashboard.button(
				string.sub(alphabet, i, i),
				project.title,
				"<cmd>lua require('telescope.builtin').find_files( { cwd = '" .. path_escape .. "' }) <cr>"
			)
			table.insert(tbl, file_button_el)
		end
		return {
			type = "group",
			val = tbl,
			opts = {},
		}
	end

	-- Layout
	config.layout[2] = random_header({ cool, panda })
	config.layout[4] = links
	config.layout[5] = { type = "padding", val = 1 }
	config.layout[6] = get_projects(5)
	config.layout[7] = { type = "padding", val = 1 }
	config.layout[8] = get_mru(9)

	require("alpha").setup(config)

	local alpha_group = vim.api.nvim_create_augroup("alpha", { clear = true })
	vim.api.nvim_create_autocmd("User", {
		group = alpha_group,
		pattern = "AlphaReady",
		command = "set laststatus=0 | set showtabline=0",
	})
	vim.api.nvim_create_autocmd("User", {
		group = alpha_group,
		pattern = "AlphaClosed",
		command = "set laststatus=3 | set showtabline=2",
	})
end

return M
