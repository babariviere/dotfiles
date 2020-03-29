{ config, lib, pkgs, ... }:

{
  imports = [
    ./android.nix
    ./elixir.nix
    ./elm.nix
    ./go.nix
    ./godot.nix
    ./haskell.nix
    ./javascript.nix
    ./latex.nix
    ./mysql.nix
    ./php.nix
    ./plantuml.nix
    ./python.nix
    ./ruby.nix
    ./rust.nix
    ./web.nix
  ];
}
