######################################################
# Install apps using Chocolatey
######################################################
Write-Host "Installing applications from Chocolatey"
choco install 7zip -y
choco install adobereader -y
choco install adobereader-update -y
choco install androidstudio -y
choco install chocolatey -y
choco install chocolateygui -y
choco install chrome -y
choco install conemu -y
choco install curl -y
choco install deluge -y
choco install dropbox -y
choco install firefox -y
choco install git -params '"/GitAndUnixToolsOnPath"' -y
choco install hwmonitor -y
choco install r.studio -y
choco install sourcetree -y
choco install thunderbird -y
choco install virtualbox -y
choco install vlc -y
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