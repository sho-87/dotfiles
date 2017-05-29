set dropbox=D:/Dropbox/dotfiles
set homeDir=C:/Users/%USERNAME%

setx HOME %homeDir%

git clone https://github.com/syl20bnr/spacemacs %homeDir%/.emacs.d

mklink "%homeDir%/.spacemacs"             "%dropbox%/spacemacs/.spacemacs"