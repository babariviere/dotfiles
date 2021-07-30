{ lib, ... }:

{
  mkFlake = import ./mk-flake.nix { inherit lib; };
}
