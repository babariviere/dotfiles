#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Usage: $0 <machine>"
    exit 1
fi

nix build --no-link -f "<nixpkgs/nixos>" -I "nixos-config=devices/$1.nix" -I "config=./config" config.system.build.toplevel
nixos-rebuild -I "nixos-config=devices/$1.nix" -I "config=./config" "${@:2}"
