-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system(
      { "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git",
          "--branch=stable", -- latest stable release
          lazypath })
end
vim.opt.rtp:prepend(lazypath)

-- Fix treesitter folds
vim.api.nvim_create_autocmd({ 'BufEnter', 'BufAdd', 'BufNew', 'BufNewFile', 'BufWinEnter' }, {
    group = vim.api.nvim_create_augroup('TS_FOLD_WORKAROUND', {}),
    callback = function()
      vim.opt.foldmethod = 'expr'
      vim.opt.foldexpr = 'nvim_treesitter#foldexpr()'
    end
})

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
        'EdenEast/nightfox.nvim',
        cond = not_vscode,
        lazy = false, -- make sure we load this during startup if it is your main colorscheme
        priority = 1000, -- make sure to load this before all the other start plugins
        config = function()
          -- load the colorscheme here
          vim.cmd([[colorscheme nordfox]])
        end
    },
    {
        'sitiom/nvim-numbertoggle',
        cond = not_vscode,
        event = "VeryLazy"
    },
    {
        'nvim-lualine/lualine.nvim',
        cond = not_vscode,
        dependencies = { 'nvim-tree/nvim-web-devicons' },
        config = function()
          require('lualine').setup()
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
              window = {
                  border = "single"
              }
          }
        end,
        event = "VeryLazy"
    },
    {
        'nvim-telescope/telescope.nvim',
        cond = not_vscode,
        dependencies = { {
            'nvim-lua/plenary.nvim',
            'nvim-tree/nvim-web-devicons',
        } },
        config = function()
          require('telescope').setup {
          }
        end,
        event = "VeryLazy"
    }
})
