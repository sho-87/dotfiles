set nocompatible              " be iMproved, required
filetype off                  " required

"""""""""""""""""""" Set path on Windows """"""""""""""""""""

" On Windows, also use '.vim' instead of 'vimfiles'; this makes
" synchronization across (heterogeneous) systems easier.
if has('win32')
    set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
    set rtp+=$HOME/.vim/bundle/Vundle.vim/
    call vundle#begin('$HOME/.vim/bundle/')
else
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()
endif


"""""""""""""""""""" Plugins """"""""""""""""""""

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Add plugins
Plugin 'scrooloose/nerdtree'
Plugin 'kien/ctrlp.vim'
Plugin 'bling/vim-airline'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-fugitive'
Plugin 'easymotion/vim-easymotion'
Plugin 'chriskempson/base16-vim'
Plugin 'Raimondi/delimitMate'
Plugin 'pangloss/vim-javascript'
Plugin 'jeetsukumaran/vim-buffergator'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


"""""""""""""""""""" Vim Options """"""""""""""""""""

" Set colour scheme and font
if has('gui_running')
  set guioptions-=T  " no toolbar
  if has('gui_win32')
    set guifont=Consolas:h11
    colorscheme base16-Twilight
  else
    set guifont=Consolas:h11
    set t_Co=256
    colorscheme monokai
  endif
endif

" Syntax highlighting
syntax on

" Auto change directory
set autochdir

" Absolute line numbers (hybrid mode)
set relativenumber
set number

" ruler
set colorcolumn=80
highlight ColorColumn ctermbg=0 guibg=lightgrey

" set spaces for tab
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

" highlight search
set gdefault      " Never have to type /g at the end of search / replace again
set ignorecase    " case insensitive searching (unless specified)
set smartcase
set hlsearch
set incsearch
set showmatch

" Use one space, not two, after punctuation.
set nojoinspaces

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Backspace deletes like most programs in insert mode
set backspace=2

" Start scrolling when we're 8 lines away from margins
set scrolloff=8 
set sidescrolloff=15
set sidescroll=1


"""""""""""""""""""" Plugin Options """"""""""""""""""""

" NERDTree close if only thing left open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" NERDTree File type highlighting
function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
 exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
 exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunction

call NERDTreeHighlightFile('jade', 'green', 'none', 'green', '#151515')
call NERDTreeHighlightFile('ini', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('md', 'blue', 'none', '#3366FF', '#151515')
call NERDTreeHighlightFile('py', 'blue', 'none', '#3366FF', '#151515')
call NERDTreeHighlightFile('yml', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('config', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('conf', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('json', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('html', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('styl', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('css', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('coffee', 'Red', 'none', 'red', '#151515')
call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', '#151515')
call NERDTreeHighlightFile('php', 'Magenta', 'none', '#ff00ff', '#151515')

" Show vim-airline all the time
set laststatus=2

" Show all tabs/buffers
let g:airline#extensions#tabline#enabled = 1


"""""""""""""""""""" Keymaps """"""""""""""""""""
" set leader key
let mapleader = " "

" toggle nerdtree
map <Leader>n :NERDTreeToggle<CR>

" Toggle CtrlP
let g:ctrlp_map = '<C-t>'
let g:ctrlp_cmd = 'CtrlP'

" Stop highlight after searching
nnoremap <silent> <leader>, :noh<cr>

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Navigate properly when lines are wrapped
nnoremap j gj
nnoremap k gk

" SYSTEM CLIPBOARD COPY & PASTE SUPPORT
set pastetoggle=<F2> "F2 before pasting to preserve indentation
"Copy paste to/from clipboard
vnoremap <C-c> "*y
map <silent><Leader>p :set paste<CR>o<esc>"*]p:set nopaste<cr>"
map <silent><Leader><S-p> :set paste<CR>O<esc>"*]p:set nopaste<cr>"
map <silent><C-v> :set paste<CR>o<esc>"*]p:set nopaste<cr>"

" Map Ctrl + S to save in any mode
noremap <silent> <C-S>          :update<CR>
vnoremap <silent> <C-S>         <C-C>:update<CR>
inoremap <silent> <C-S>         <C-O>:update<CR>

" buffer splits
nmap <leader>h  :leftabove  vnew<CR>
nmap <leader>l  :rightbelow vnew<CR>
nmap <leader>k  :leftabove  new<CR>
nmap <leader>j  :rightbelow new<CR>

" remap escape/normal mode
:imap jj <Esc>