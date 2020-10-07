#!/usr/bin/env bash

set -e

dir="$HOME/src/github.com/babariviere/dotfiles"

info() {
  echo -e "\033[1minfo:\033[22m" $@
}

pushd "$dir" >/dev/null

if [ ! $(command -v brew) ]; then
  info "Installing brew"
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
else
  info "Brew is installed"
fi

info "Installing Brewfile"
brew bundle install

if [ ! $(command -v rustc) ]; then
  info "Installing rust"
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  source $HOME/.cargo/env
else
  info "Rust is installed"
fi

info "Please, refresh your shell by doing either:"
info '- executing: `source $HOME/.zshrc`'
info '- restart your shell'

popd >/dev/null
