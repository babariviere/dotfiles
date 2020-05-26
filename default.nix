{ config, lib, pkgs, ... }:

# Default configuration for dotfiles
let sources = import ./nix/sources.nix;
in {
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    "${sources.home-manager}/nixos"

    ./themes
    ./modules
  ];

  dotfiles.name = "Bastien Rivière";

  home-manager.users."${config.dotfiles.user}".home.sessionVariables =
    import ./env.nix { inherit lib; };
}
