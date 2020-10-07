#!/bin/sh

common="$HOME/src/github.com/babariviere/dotfiles/_system/common"

brew update
brew upgrade --fetch-head
brew cleanup

sh "$common/asdf.sh"
sh "$common/nvim.sh"
mas upgrade
