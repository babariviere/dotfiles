#!/usr/bin/env bash

dir="$HOME/src/github.com/babariviere/dotfiles"

pushd "$dir" >/dev/null

if [ ! $(command -v brew) ]; then
  info "Installing brew"
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
else
  info "Brew is installed"
fi

info "Checking if Brewfile dependencies are installed"
if [ ! $(brew bundle check >/dev/null 2>/dev/null) ]; then
  info "Installing Brewfile dependecies"
  brew bundle install
else
  info "Dependencies are installed"
fi

popd >/dev/null
