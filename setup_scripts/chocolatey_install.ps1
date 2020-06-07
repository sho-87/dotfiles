######################################################
# Install apps using Chocolatey
######################################################
Write-Host "Installing applications from Chocolatey"
choco install 7zip -y
choco install adobereader -y
choco install adobereader-update -y
choco install authy-desktop -y
choco install chocolatey -y
choco install dropbox -y
choco install firacode -y
choco install firefox -y
choco install git --params '"/GitAndUnixToolsOnPath"' -y
choco install gitkraken -y
choco install googlechrome -y
choco install hwmonitor -y
choco install nodejs-lts -y
choco install powertoys -y
choco install pushbullet -y
choco install r.studio -y
choco install virtualbox -y
choco install visualstudiocode --params '"/NoDesktopIcon /NoQuicklaunchIcon"' -y
choco install vlc -y
choco install yarn -y
choco install zotero-standalone -y
Write-Host

######################################################
# Configure Git globals
######################################################
Write-Host "Configuring Git globals"
$userName = Read-Host 'Enter your name for git configuration'
$userEmail = Read-Host 'Enter your email for git configuration'

& 'C:\Program Files\Git\bin\git' config --global user.email $userEmail
& 'C:\Program Files\Git\bin\git' config --global user.name $userName
