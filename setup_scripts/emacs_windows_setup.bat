set dropbox=D:/Dropbox/dotfiles
set homeDir=C:/Users/%USERNAME%

REM setx HOME %homeDir%

mklink "%homeDir%/.emacs.d/init.el" "%dropbox%/emacs/init.el"