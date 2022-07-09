{ config, lib, pkgs, ... }:

{
  home.packages =
    [ pkgs.nixfmt pkgs.nix-prefetch-scripts pkgs.nixpkgs-review pkgs.nix-tree ];
}
