#!/bin/sh

brew update
brew upgrade --fetch-head
brew cleanup

# TODO: meh
dir="$HOME/src/github.com/babariviere/dotfiles"

pushd "$dir" >/dev/null
brew bundle dump -f --all
popd >/dev/null
