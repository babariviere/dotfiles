#!/bin/sh

set -e

hm=$(pwd)/.home/doom-emacs
if [ ! -d $hm/.emacs.d ]; then
  git clone --depth 1 https://github.com/hlissner/doom-emacs $hm/.emacs.d
fi
rsync -rtv $(pwd)/config/doom.d/ $hm/.doom.d/
# For vterm
rsync -rtv $(pwd)/config/zshrc $hm/.zshrc
env HOME=$hm nix shell .#emacsGit -c $hm/.emacs.d/bin/doom sync
env HOME=$hm nix shell .#emacsGit -c emacs --debug --debug-init
