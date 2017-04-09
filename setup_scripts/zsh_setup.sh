#!/bin/bash

sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

cd ~

ln -s ~/Dropbox/dotfiles/.zshrc .zshrc