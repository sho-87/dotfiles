######################################################
# Install apps using Chocolatey
######################################################
Write-Host "Installing applications from Chocolatey"
# choco install 7zip -y
choco install adobereader -y
choco install adobereader-update -y
choco install authy-desktop -y
choco install bitwarden -y
choco install calibre -y
choco install dbeaver -y
choco install discord -y
choco install docker-desktop -y
choco install dropbox -y
choco install ffmpeg -y
choco install firacode -y
choco install git --params '"/GitAndUnixToolsOnPath"' -y
choco install gitkraken -y
choco install googlechrome -y
choco install hwmonitor -y
choco install libreoffice-fresh -y
choco install obs-studio -y
choco install parsec -y
choco install powertoys -y
choco install r -y
choco install r.studio -y
choco install rufus -y
choco install slack -y
choco install steam-client -y
# choco install thunderbird -y
choco install vscode --params '"/NoDesktopIcon /NoQuicklaunchIcon"' -y
choco install vlc -y
choco install zoom -y
choco install zotero -y

######################################################
# Configure Git globals
######################################################
Write-Host "Configuring Git globals"
$userName = Read-Host 'Enter your name for git configuration'
$userEmail = Read-Host 'Enter your email for git configuration'

& 'C:\Program Files\Git\bin\git' config --global user.email $userEmail
& 'C:\Program Files\Git\bin\git' config --global user.name $userName
