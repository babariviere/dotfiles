{ config, lib, pkgs, ... }:

# Default configuration for dotfiles
{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    <home-manager/nixos>

    ./themes
    ./modules
  ];

  dotfiles.name = "Bastien Rivière";
}
