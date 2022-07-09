{ nix }:

nix.overrideAttrs
(old: { patches = old.patches or [ ] ++ [ ./add-flake-nix.patch ]; })
