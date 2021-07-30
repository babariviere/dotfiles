{ lib, ... }:

{
  mkFlake = import ./mk-flake { inherit lib; };
}
