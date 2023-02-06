winget install -e --id 7zip.7zip
winget install -e --id Adobe.Acrobat.Reader.64-bit
winget install -e --id Anaconda.Anaconda3
winget install -e --id Twilio.Authy
winget install -e --id Bitwarden.Bitwarden
winget install -e --id Discord.Discord
winget install -e --id Docker.DockerDesktop
winget install -e --id Google.Drive
winget install -e --id Dropbox.Dropbox
winget install -e --id Mozilla.Firefox
winget install -e --id GIMP.GIMP
winget install -e --id Git.Git
winget install -e --id Axosoft.GitKraken
winget install -e --id Google.Chrome
winget install -e --id TheDocumentFoundation.LibreOffice
winget install -e --id OBSProject.OBSStudio
winget install JanDeDobbeleer.OhMyPosh -s winget
winget install -e --id Parsec.Parsec
winget install -e --id Microsoft.PowerToys
winget install -e --id Nvidia.GeForceExperience
winget install -e --id RProject.R
winget install -e --id Valve.Steam
winget install -e --id Microsoft.VisualStudioCode
winget install -e --id VideoLAN.VLC
winget install -e --id DigitalScholar.Zotero

######################################################
# Configure Git globals
######################################################
Write-Host "Configuring Git globals"
$userName = Read-Host 'Enter your name for git configuration'
$userEmail = Read-Host 'Enter your email for git configuration'

& 'C:\Program Files\Git\bin\git' config --global user.email $userEmail
& 'C:\Program Files\Git\bin\git' config --global user.name $userName