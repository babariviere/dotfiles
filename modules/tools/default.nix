{ config, lib, pkgs, ... }:

{
  imports = [ ./aws.nix ./build.nix ./devops.nix ./insomnia.nix ];
}
