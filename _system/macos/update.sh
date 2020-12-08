#!/usr/bin/env bash

dir="$HOME/src/github.com/babariviere/dotfiles"
common="$dir/_system/common"

brew update
brew upgrade --fetch-head
brew cleanup

for f in $common/update/*; do
  echo "Updating $f..."
  sh "$f"
done

mas upgrade

pushd "$dir" >/dev/null
brew bundle dump -f --all
popd >/dev/null
