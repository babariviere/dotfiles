#!/usr/bin/env bash

set -e

dir="$HOME/src/github.com/babariviere/dotfiles"

info() {
  echo -e "\033[1minfo:\033[22m" $@
}
export -f info

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

info "Installing dotfiles"
rcup -v

sh "$dir/_system/common/setup/rust.sh"
sh "$dir/_system/common/setup/go.sh"
sh "$dir/_system/common/setup/tmux.sh"

pushd "$HOME" >/dev/null
info "Allowing envrc"
direnv allow
popd

info "Linking iCloud to home"
ln -s $HOME/Library/Mobile\ Documents/com\~apple\~CloudDocs $HOME/iCloud

echo ""
info "Please, refresh your shell by doing either:"
info '- executing: `source $HOME/.zshrc`'
info '- restart your shell'

popd >/dev/null
