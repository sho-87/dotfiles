-- Bootstrap packer
local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

-- Autocompile packer
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

-- Fix treesitter folds
vim.api.nvim_create_autocmd({'BufEnter', 'BufAdd', 'BufNew', 'BufNewFile', 'BufWinEnter'}, {
    group = vim.api.nvim_create_augroup('TS_FOLD_WORKAROUND', {}),
    callback = function()
        vim.opt.foldmethod = 'expr'
        vim.opt.foldexpr = 'nvim_treesitter#foldexpr()'
    end
})

-- Plugins
local vscode = vim.g.vscode == 1

return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'
    use 'tpope/vim-surround'
    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup()
        end
    }
    use {
        'phaazon/hop.nvim',
        branch = 'v2',
        config = function()
            require('hop').setup {
                keys = 'etovxqpdygfblzhckisuran',
                multi_windows = true
            }
        end
    }
    use {
        'nvim-treesitter/nvim-treesitter',
        config = function()
            require'nvim-treesitter.install'.compilers = {"mingw", "clang", "gcc"}
            require'nvim-treesitter.configs'.setup {
                ensure_installed = {"c", "lua", "vim", "help", "cpp", "css", "comment", "dockerfile", "gitattributes",
                                    "gitcommit", "go", "hlsl", "html", "java", "javascript", "json", "json5", "julia",
                                    "kotlin", "latex", "markdown", "markdown_inline", "python", "r", "regex", "rust",
                                    "typescript", "yaml"},
                highlight = {
                    enable = true,
                    additional_vim_regex_highlighting = false
                }
            }
        end
    }

    if not vscode then
        use {'EdenEast/nightfox.nvim'}
        use {'sitiom/nvim-numbertoggle'}
        use {
            'nvim-lualine/lualine.nvim',
            requires = {'nvim-tree/nvim-web-devicons'},
            config = function()
                require('lualine').setup()
            end
        }
        use {
            'mvllow/modes.nvim',
            config = function()
                require('modes').setup()
            end
        }
        use {
            'lukas-reineke/indent-blankline.nvim',
            config = function()
                require('indent_blankline').setup()
            end
        }
    end

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if packer_bootstrap then
        require('packer').sync()
    end
end)
