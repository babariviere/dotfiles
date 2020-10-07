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

info "Checking if Brewfile dependencies are installed"
if [ ! $(brew bundle check >/dev/null 2>/dev/null) ]; then
  info "Installing Brewfile dependecies"
  brew bundle install
else
  info "Dependencies are installed"
fi

info "Installing dotfiles"
rcup -v

if [ ! $(command -v rustc) ]; then
  info "Installing rust"
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  source $HOME/.cargo/env
else
  info "Rust is installed"
fi

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
echo
info "Please, refresh your shell by doing either:"
info '- executing: `source $HOME/.zshrc`'
info '- restart your shell'

popd >/dev/null
