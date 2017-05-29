set dropbox=D:/Dropbox/dotfiles
set backupcopy=yes

mklink /D %USERPROFILE%/.vim             %dropbox%/vim
echo source %dropbox%/vim/vimrc.vim > %USERPROFILE%/.vimrc
echo source %dropbox%/vim/gvimrc.vim > %USERPROFILE%/.gvimrc