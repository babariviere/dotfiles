{ writeShellScriptBin }:

writeShellScriptBin "nixos-update" ''
  set -e

  nixpkgs="$(nix eval --raw '(import ./nix/sources.nix).nixpkgs')"
  nix-store --realise "$nixpkgs" 2>/dev/null

  nix build --no-link -f '$nixpkgs/nixos' config.system.build.toplevel
  nixos-rebuild -I "nixpkgs=$nixpkgs" $@
''
