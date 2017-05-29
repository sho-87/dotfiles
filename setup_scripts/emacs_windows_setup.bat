set dropbox=D:/Dropbox/dotfiles
set homeDir=C:/Users/%USERNAME%

REM setx HOME %homeDir%

if not exist "%homeDir%/.emacs.d/" mkdir %homeDir%/.emacs.d

mklink "%homeDir%/.emacs.d/init.el" "%dropbox%/emacs/init.el"
mklink "%homeDir%/.emacs.d/custom.el" "%dropbox%/emacs/custom.el"