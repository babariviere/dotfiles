#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash nixos-generators


if [ -z "$1" ]; then
    echo "Usage: $0 <machine>"
    exit 1
fi
nixos-generate -f iso -c "configuration.$1.nix"
