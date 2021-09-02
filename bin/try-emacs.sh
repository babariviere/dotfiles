#!/bin/sh

set -e

hm=$(pwd)/.home/amber-emacs
rsync -rtv $(pwd)/config/emacs.d/ $hm/.emacs.d/
# For vterm
rsync -rtv $(pwd)/config/zshrc $hm/.zshrc
env HOME=$hm nix run .#amber-emacs -- --debug --debug-init
