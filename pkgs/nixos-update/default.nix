{ writeShellScriptBin }:

writeShellScriptBin "nixos-update" ''
  set -e

  nix build --no-link -f "<nixpkgs/nixos>" config.system.build.toplevel "''${@:2}"
  nixos-rebuild $@
''
