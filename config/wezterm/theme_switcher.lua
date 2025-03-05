local M = {}
local hour = os.date("*t").hour
M.color_scheme = (hour >= 7 and hour < 19) and "kanagawa-paper-canvas" or "kanagawa-paper-ink"

--- CONFIG BEGIN ---
-- default colorscheme after neovim exits
local theme_default = M.color_scheme

-- this should match the path set in the neovim config
-- it's best to use a temporary directory for this
local theme_file = "/tmp/nvim-theme"

-- relative path to the directory containing the tabline themes
-- e.g. if I have placed the tabline extra themes in ./colors/wezterm_tabline then this would be "colors.wezterm_tabline"
-- this is treated as a relative lua module that will be required by the theme switcher
local tabline_theme_dir = "colors.wezterm_tabline"
--- CONFIG END ---

local wezterm = require("wezterm")

-- NOTE: file must exist for watch list reloading to work
local f = io.open(theme_file, "r") ~= nil
if not f then
	io.open(theme_file, "w"):close()
end
wezterm.add_to_config_reload_watch_list(theme_file)

M.plugins = wezterm.plugin.list()

function M.is_plugin_installed(url)
	for _, p in ipairs(M.plugins) do
		if p.url and string.find(p.url, url, 1, true) then
			return true
		end
	end
	return false
end

function M.read_theme()
	local file = io.open(theme_file, "r")
	if file then
		local theme = file:read("*all"):gsub("\n", "")
		file:close()
		if theme == "" then
			return nil
		end
		return theme
	end
	return nil
end

function M.apply_theme(window, theme)
	window:set_config_overrides({
		color_scheme = theme,
	})
end

function M.apply_tabline_theme(theme)
	local tabline = wezterm.plugin.require("https://github.com/michaelbrusegard/tabline.wez")
	local ok, theme_module = pcall(require, tabline_theme_dir .. "." .. theme)
	if ok and theme_module.theme_overrides then
		tabline.setup({
			options = {
				theme_overrides = theme_module.theme_overrides,
			},
		})
	end
end

wezterm.on("window-config-reloaded", function(window, _)
	local new_theme = M.read_theme()
	if not new_theme then
		new_theme = theme_default
	end

	M.apply_theme(window, new_theme)

	if M.is_plugin_installed("michaelbrusegard/tabline.wez") then
		M.apply_tabline_theme(new_theme)
	end
end)

return M
