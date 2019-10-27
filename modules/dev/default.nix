{ config, lib, pkgs, ... }:

{
  imports = [ ./go.nix ./rust.nix ];
}
