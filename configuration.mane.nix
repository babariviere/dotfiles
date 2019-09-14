{ config, lib, pkgs, ... }:

{
  imports = [
    ./.

    ./modules/desktop
    ./modules/editors/emacs.nix
    ./modules/services/syncthing.nix
  ];

  services.dotfiles = {
    user = "babariviere";
  };
}
