#!/bin/sh

asdf update
asdf plugin-update --all

# Update packages to latest version
for pkg in $(asdf plugin list); do
  latest=$(asdf latest "$pkg")
  asdf install "$pkg" "$latest"
  asdf global "$pkg" "$latest"
done
