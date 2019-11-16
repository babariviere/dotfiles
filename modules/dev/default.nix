{ config, lib, pkgs, ... }:

{
  imports = [ ./go.nix ./haskell.nix ./mysql.nix ./rust.nix ];
}
