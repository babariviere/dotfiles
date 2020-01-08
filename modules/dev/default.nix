{ config, lib, pkgs, ... }:

{
  imports = [
    ./android.nix
    ./go.nix
    ./godot.nix
    ./haskell.nix
    ./latex.nix
    ./mysql.nix
    ./plantuml.nix
    ./rust.nix
  ];
}
