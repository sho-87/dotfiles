######################################################
# Install apps using Chocolatey
######################################################
Write-Host "Installing applications from Chocolatey"
choco install adobereader -y
choco install adobereader-update -y
choco install chocolatey -y
choco install chrome -y
choco install conemu -y
choco install curl -y
choco install deluge -y
choco install dropbox -y
choco install firefox -y
choco install git -params '"/GitAndUnixToolsOnPath"' -y
choco install r.studio -y
choco install sourcetree -y
choco install thunderbird -y
choco install virtualbox -y
choco install vlc -y
choco install zotero-standalone -y --version 5.0.7
choco pin -n zotero-standalone --version 5.0.7
Write-Host