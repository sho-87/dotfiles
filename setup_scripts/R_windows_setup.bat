set dropbox=D:/Dropbox/dotfiles
set homeDir=C:/Users/%USERNAME%

setx R_USER %homeDir%

mklink "%homeDir%/.Rprofile" "%dropbox%/.Rprofile"