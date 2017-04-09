" Use Vim settings, rather than Vi settings. This must be first
set nocompatible

" ================= Plugins ==================
call plug#begin()

Plug 'freeo/vim-kalisi'
Plug 'sickill/vim-monokai'

call plug#end()

" ============= Colours ===============
colorscheme kalisi " choose colour scheme
set background=dark " set theme of the colour scheme
syntax on " syntax highlighting

highlight NonText guifg=#f442e8 " change colour of invisibles
highlight SpecialKey guifg=#f442e8 " change colour of invisibles
highlight ColorColumn guibg=black " colorcolumn colour (gui)
highlight ColorColumn ctermbg=black " colorcolumn colour (terminal)

" ============= General settings =============
filetype plugin indent on " Enable file type detection with language based indenting

set autoindent " autoindent lines
set backspace=indent,eol,start " allow backspace across lines and indents
set colorcolumn=120 " show colour line at a certain column/width
set cursorline " highlight current line
set encoding=utf-8 " use utf-8 encoding
set expandtab " use spaces when tabbing
set ffs=dos,unix " File format
set hidden " allow hidden buffers without having to close the current one
set hlsearch " highlight searched text
set history=100 " number of previous commands to store
set ignorecase " ignore case when searching
set incsearch " search as characters are entered
set lazyredraw " dont redraw screen during macro and script execution
set list listchars=tab:▸-,trail:▾ " set invisible characters
set mouse=a " enable mouse with ALT and click
set nobackup " dont use temporary backup files
set noerrorbells " disable beep on errors
set nojoinspaces " no extra space after . when joining lines
set number " show current line number
set relativenumber " relative line numbers
set ruler " enable ruler for current position
set scrolloff=3 " vertical scroll offset (# of lines to show below)
set shiftround " use a multiple of shiftwidth when indenting with < and >
set shiftwidth=4 " number of spaces for autoindenting
set showcmd " show last command in bottom bar
set showmatch " highlight matching brackets
set smartcase " ignore case unless capital is searched
set smarttab " insert tabstop number of spaces when pressing tab
set splitbelow " horizontal split below current
set splitright " vertical split to the right of current
set tabstop=4 " indent using certain number of spaces
set textwidth=120 " wrap lines automatically at a certain column value
set title " set window title to reflect filename
set wildmenu " visual autocomplete for command menu
set wildignore=*.pyc " ignore certain extensions in command menu

" ============= Keybindings =============
let mapleader='\' " set leader key
nnoremap <Leader>rr :source $MYVIMRC<cr> " Reload vimrc without restart
nnoremap <silent> <Esc> :nohlsearch<Bar>:echo<CR> " Cancel search with escape
