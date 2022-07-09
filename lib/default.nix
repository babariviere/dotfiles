{ lib, ... }:

{
  mkFlake = import ./mk-flake { inherit lib; };

  foldProfiles = import ./fold-profiles.nix { inherit lib; };
}
