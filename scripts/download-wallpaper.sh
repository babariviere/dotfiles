#!/usr/bin/env -S guix shell librsvg wget bash -- bash

set -Eeuo pipefail

wallpaper_dir=${XDG_PICTURES_DIR:-$HOME/Pictures}/backgrounds
mkdir -p "$wallpaper_dir"
wget -O /tmp/blob.svg https://gitlab.gnome.org/GNOME/gnome-backgrounds/-/raw/main/backgrounds/blobs-d.svg?inline=false
rsvg-convert -h 2560 -w 1440 /tmp/blob.svg -o "$wallpaper_dir/blobs-d.jpg"
