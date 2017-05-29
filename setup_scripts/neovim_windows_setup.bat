set dropbox=D:/Dropbox/dotfiles
set backupcopy=yes

REM symlink dotfile directory
mklink /D %userprofile%/AppData/Local/nvim             %dropbox%/neovim

REM source the dropbox init.vim file in the neovim runtime path
echo source %dropbox%/neovim/init.vim > %userprofile%/AppData/Local/nvim/init.vim

REM create autoload directory
mkdir %userprofile%\AppData\Local\nvim\autoload

REM download vim-plug manager using powershell
powershell -command "(New-Object Net.WebClient).DownloadFile('https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim', $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath('%userprofile%/AppData/Local/nvim/autoload/plug.vim'))"

REM setup IdeaVIM
echo source %dropbox%/neovim/init.vim > %userprofile%/.ideavimrc