#!/usr/bin/env bash

dirs=(
  "$HOME/.bin"
  "$HOME/.config"
  "$HOME/.hammerspoon"
  "$HOME/.zsh"
)

for d in "${dirs[@]}"; do
  find -L "$d" -type l -exec unlink {} \;
done
