#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Usage: $0 <machine>"
    exit 1
fi
nixpkgs="$(nix eval --raw '(import /etc/dotfiles/nix/sources.nix).nixpkgs')"
nix-store --realise "$nixpkgs" 2>/dev/null

nix build --no-link -f "$nixpkgs/nixos" -I "nixpkgs=$nixpkgs" -I "nixos-config=devices/$1.nix" -I "config=./config" config.system.build.toplevel
nixos-rebuild -I "nixpkgs=$nixpkgs" -I "nixos-config=devices/$1.nix" -I "config=./config" "${@:2}"
