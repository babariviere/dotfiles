{ config, lib, pkgs, ... }:

{
  imports = [
    ./.
    ./modules/services/syncthing.nix
  ];

  services.dotfiles = {
    user = "babariviere";
  };
}
