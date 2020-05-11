{ config, lib, pkgs, ... }:

let dotfiles = config.dotfiles;
in {
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
    ./lua.nix
    ./mysql.nix
    ./ocaml.nix
    ./php.nix
    ./plantuml.nix
    ./python.nix
    ./ruby.nix
    ./rust.nix
    ./web.nix
  ];

  home-manager.users."${dotfiles.user}" = {
    home.file = { ".editorconfig".source = <config/editorconfig>; };
  };
}
