{ config, lib, pkgs, ... }:

{
  imports = [
    ./android.nix
    ./go.nix
    ./godot.nix
    ./haskell.nix
    ./javascript.nix
    ./latex.nix
    ./mysql.nix
    ./plantuml.nix
    ./python.nix
    ./rust.nix
  ];
}
