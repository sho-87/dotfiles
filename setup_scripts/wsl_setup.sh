#!/bin/bash

cd ~

# Create links to host directories
ln -sn /mnt/d/Documents ~/Documents
ln -sn /mnt/d/Downloads ~/Downloads
ln -sn /mnt/d/Dropbox ~/Dropbox
ln -sn /mnt/f ~/Git

# Setup zsh
sudo apt-get -y install zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
rm ~/.zshrc
ln -s ~/Dropbox/dotfiles/.zshrc ~/.zshrc
