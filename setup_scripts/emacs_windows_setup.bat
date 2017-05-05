set dropbox=D:/Dropbox/dotfiles
set homeDir=C:/Users/%USERNAME%

REM setx HOME %homeDir%

mklink "%homeDir%/.emacs.el" "%dropbox%/emacs/.emacs.el"
mklink "%homeDir%/.emacs_packages.el" "%dropbox%/emacs/.emacs_packages.el"
mklink "%homeDir%/.emacs_keybinds.el" "%dropbox%/emacs/.emacs_keybinds.el"