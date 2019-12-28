{ config, lib, pkgs, ... }:

{
  imports = [
    ./android.nix
    ./go.nix
    ./haskell.nix
    ./latex.nix
    ./mysql.nix
    ./plantuml.nix
    ./rust.nix
  ];
}
