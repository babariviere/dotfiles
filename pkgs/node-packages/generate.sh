#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nodePackages.node2nix
set -eu -o pipefail

node2nix -i packages.json -o packages.nix
