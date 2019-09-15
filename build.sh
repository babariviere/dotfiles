#!/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $0 <machine>"
    exit 1
fi
nix-build "<nixpkgs/nixos>" -A vm -k -I "nixos-config=configuration.$1.nix" -I "nixpkgs=$HOME/projects/nixpkgs" "${@:2}"
