#!/bin/sh

rsync -rtv $(pwd)/config/emacs.d/ $(pwd)/.emacs.d/
# For vterm
rsync -rtv $(pwd)/config/zshrc $(pwd)/.zshrc
env HOME=$(pwd) nix run .#amber-emacs -- --debug --debug-init
