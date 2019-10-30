{ config, lib, pkgs, ... }:

{
  imports = [
    ./aws.nix
    ./build.nix
    ./docker.nix
    ./devops.nix
    ./insomnia.nix
    ./light.nix
  ];
}
