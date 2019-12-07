#!/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $0 <machine>"
    exit 1
fi
nix-build "<nixpkgs/nixos>" -A vm -k -I "nixos-config=devices/$1.nix" -I "config=./config" "${@:2}"
