#!/bin/sh

move_icon() {
  echo "==> Moving $1"
  cp "$HOME/iCloud/Assets/hammerspoon/$1.png" "$HOME/src/github.com/babariviere/dotfiles/hammerspoon/assets/$1.png"
}

export_icon() {
  echo "==> Exporting $1"
  inkscape --export-type png \
    --export-filename "$HOME/src/github.com/babariviere/dotfiles/hammerspoon/assets/$1.png" \
      -w 16 "$HOME/iCloud/Assets/hammerspoon/$1.svg"
}

move_icon tile_active
move_icon tile_inactive
