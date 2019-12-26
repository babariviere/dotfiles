{ config, lib, pkgs, ... }:

{
  imports =
    [ ./android.nix ./go.nix ./haskell.nix ./latex.nix ./mysql.nix ./rust.nix ];
}
