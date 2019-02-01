#!/bin/bash

cd ~

# Create links to host directories
ln -sn /mnt/d/Documents ~/Documents
ln -sn /mnt/d/Downloads ~/Downloads
ln -sn /mnt/d/Dropbox ~/Dropbox
ln -sn /mnt/f ~/Git

# Setup zsh
sudo apt-get -y install zsh
rm ~/.zshrc # if rerunning this script with an existing install
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sed 's:env zsh -l::g' | sed 's:chsh -s .*$::g')"
rm ~/.zshrc
ln -s ~/Dropbox/dotfiles/.zshrc ~/.zshrc

# Get additional zsh plugins
git clone https://github.com/zsh-users/zsh-completions ~/.oh-my-zsh/custom/plugins/zsh-completions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-history-substring-search ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-history-substring-search
