-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system(
      { "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git",
          "--branch=stable",
          lazypath })
end
vim.opt.rtp:prepend(lazypath)

-- Plugins
local not_vscode = vim.g.vscode == nil

require("lazy").setup({
    {
        'tpope/vim-surround',
        event = "VeryLazy"
    },
    {
        'numToStr/Comment.nvim',
        config = function()
          require('Comment').setup()
        end,
        event = "VeryLazy"
    },
    {
        'phaazon/hop.nvim',
        branch = 'v2',
        config = function()
          require('hop').setup {
              keys = 'etovxqpdygfblzhckisuran',
              multi_windows = true
          }
        end,
        event = "VeryLazy"
    },
    {
        'mrjones2014/smart-splits.nvim',
        cond = not_vscode,
        config = function()
          require('smart-splits').setup({
              default_amount = 3,
              wrap_at_edge = false,
              move_cursor_same_row = true,
              resize_mode = {
                  quit_key = '<ESC>',
                  resize_keys = { 'h', 'j', 'k', 'l' },
                  silent = false,
              },
              ignored_events = {
                  'BufEnter',
                  'WinEnter',
              },
          })
        end,
        event = "VeryLazy"
    },
    {
        'nvim-treesitter/nvim-treesitter',
        cond = not_vscode,
        config = function()
          require 'nvim-treesitter.install'.compilers = { "mingw", "clang", "gcc" }
          require 'nvim-treesitter.configs'.setup {
              ensure_installed = { "c", "lua", "vim", "help", "cpp", "css", "comment", "dockerfile", "gitattributes",
                  "gitcommit", "go", "hlsl", "html", "java", "javascript", "json", "json5", "julia",
                  "kotlin", "latex", "markdown", "markdown_inline", "python", "r", "regex", "rust",
                  "typescript", "yaml" },
              highlight = {
                  enable = true,
                  additional_vim_regex_highlighting = false
              }
          }
        end,
        event = { "BufReadPost", "BufNewFile" }
    },
    {
        'dstein64/vim-startuptime',
        cond = not_vscode,
        cmd = "StartupTime"
    },
    {
        'rose-pine/neovim',
        name = 'rose-pine',
        cond = not_vscode,
        lazy = false, -- make sure we load this during startup if it is your main colorscheme
        priority = 1000, -- make sure to load this before all the other start plugins
        config = function()
          require("rose-pine").setup {
              dark_variant = 'moon',
              bold_vert_split = true,
              dim_nc_background = false,
              disable_italics = true,
              highlight_groups = {
                  IndentBlanklineChar = { fg = 'highlight_low' },
              }
          }

          -- load the colorscheme after config
          vim.cmd([[colorscheme rose-pine]])
        end
    },
    {
        'sitiom/nvim-numbertoggle',
        cond = not_vscode,
        event = "VeryLazy"
    },
    {
        'kevinhwang91/nvim-ufo',
        cond = not_vscode,
        dependencies = { 'kevinhwang91/promise-async' },
        config = function()
          require('ufo').setup {
              provider_selector = function(bufnr, filetype, buftype)
                return { 'treesitter', 'indent' }
              end
          }
        end,
        event = "VeryLazy"
    },
    {
        'nvim-lualine/lualine.nvim',
        cond = not_vscode,
        dependencies = { 'nvim-tree/nvim-web-devicons' },
        config = function()
          require('lualine').setup {
              extensions = { 'neo-tree' }
          }
        end,
        event = { "BufReadPost", "BufNewFile" }
    },
    {
        'mvllow/modes.nvim',
        cond = not_vscode,
        config = function()
          require('modes').setup()
        end,
        event = "VeryLazy"
    },
    {
        'lukas-reineke/indent-blankline.nvim',
        cond = not_vscode,
        config = function()
          require('indent_blankline').setup {
              space_char_blankline = " ",
              show_current_context = true,
              show_current_context_start = true,
          }
        end,
        event = { "BufReadPost", "BufNewFile" }
    },
    {
        "folke/which-key.nvim",
        cond = not_vscode,
        config = function()
          require("which-key").setup {
              defaults = {
                  color_devicons = true,
              },
              window = {
                  border = "single"
              },
          }
        end,
        event = "VeryLazy"
    },
    {
        'nvim-telescope/telescope.nvim',
        cond = not_vscode,
        dependencies = { {
            -- consider telescope-fzf-native
            'nvim-lua/plenary.nvim',
            'nvim-tree/nvim-web-devicons',
            'nvim-telescope/telescope-project.nvim'
        } },
        config = function()
          require('telescope').setup {
              extensions = {
                  project = {
                      base_dirs = {
                          '~',
                          'd:\\',
                          'f:\\'
                      },
                      sync_with_nvim_tree = true,
                  }
              }
          }
        end,
        event = "VeryLazy"
    },
    {
        "nvim-neo-tree/neo-tree.nvim",
        cond = not_vscode,
        branch = "v2.x",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-tree/nvim-web-devicons",
            "MunifTanjim/nui.nvim",
        },
        config = function()
          require("neo-tree").setup({
              close_if_last_window = true, -- Close Neo-tree if it is the last window left in the tab
              popup_border_style = "rounded",
              enable_git_status = true,
              sort_case_insensitive = true, -- used when sorting files and directories in the tree
              window = {
                  position = "left",
                  width = 37,
              },
              source_selector = {
                  winbar = true
              },
              filesystem = {
                  filtered_items = {
                      visible = true, -- when true, they will just be displayed differently than normal items
                      hide_dotfiles = false,
                      hide_gitignored = false,
                      hide_hidden = true, -- only works on Windows for hidden files/directories
                  },
                  follow_current_file = true,
                  hide_by_name = {
                      ".DS_Store",
                      "thumbs.db"
                  },
                  use_libuv_file_watcher = true, -- This will use the OS level file watchers to detect changes
              },
              git_status = {
                  window = {
                      position = "float",
                  }
              }
          })
        end,
        event = "VeryLazy"
    },
    { 'echasnovski/mini.nvim',
        cond = not_vscode,
        version = false,
        config = function()
          require('mini.basics').setup {
              options = {
                  extra_ui = true, -- Extra UI features ('winblend', 'cmdheight=0', ...)
                  win_borders = 'single', -- Presets for window borders ('single', 'double', ...)
                  move_with_alt = true, -- Move cursor in Insert, Command, and Terminal mode with <M-hjkl>
              },
          }
          require('mini.animate').setup {
              cursor = { enable = false, }
          }
          require('mini.move').setup()
          require('mini.cursorword').setup()
          require('mini.pairs').setup()
        end,
        event = "VeryLazy"
    }
})
