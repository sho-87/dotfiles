" neovim rc

" errors flash screen rather than emit beep
set visualbell

" line numbers and distances
set relativenumber 
set number

" number of lines offset when jumping
set scrolloff=2

" statusline indicates insert or normal mode
set showmode showcmd

" highlight matching parens, braces, brackets, etc
set showmatch

" http://vim.wikia.com/wiki/Searching
set hlsearch incsearch ignorecase smartcase

let mapleader = " "

" remove search highlight
nmap <esc><esc> :noh<return>

" copy to end of line
nnoremap Y y$

" keep cursor centered
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap J mzJ`z

" undo to breakpoints
inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap ! !<c-g>u
inoremap ? ?<c-g>u
