{ config, lib, pkgs, ... }:

{
  imports = [
    ./aws.nix
    ./build.nix
    ./docker.nix
    ./devops.nix
    ./insomnia.nix
    ./light.nix
    ./sql.nix
  ];

  # Universal tools
  environment.systemPackages = with pkgs; [ zip unzip jq gzip ];
}
