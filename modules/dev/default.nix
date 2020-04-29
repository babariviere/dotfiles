{ config, lib, pkgs, ... }:

{
  imports = [
    ./android.nix
    ./c.nix
    ./elixir.nix
    ./elm.nix
    ./go.nix
    ./godot.nix
    ./guile.nix
    ./haskell.nix
    ./javascript.nix
    ./latex.nix
    ./mysql.nix
    ./ocaml.nix
    ./php.nix
    ./plantuml.nix
    ./python.nix
    ./ruby.nix
    ./rust.nix
    ./web.nix
  ];
}
