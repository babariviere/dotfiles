#!/bin/sh

rsync -rtv $(pwd)/config/emacs.d/ $(pwd)/.emacs.d/
env HOME=$(pwd) nix run .#amber-emacs
