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

info "Adding asdf plugins"
asdf plugin-add erlang || true
asdf plugin-add elixir || true
asdf plugin-add jsonnet || true
asdf plugin-add nodejs || true
asdf plugin-add golang || true
asdf plugin-add yarn || true
asdf plugin-add direnv || true
bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring
info "Plugins are installed"
info "Installing asdf packages"
asdf install
info "asdf packages are installed"

pushd "$HOME" >/dev/null
info "Allowing envrc"
direnv allow
popd

echo ""
info "Please, refresh your shell by doing either:"
info '- executing: `source $HOME/.zshrc`'
info '- restart your shell'

popd >/dev/null
