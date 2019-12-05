{ config, lib, pkgs, ... }:

{
  imports = [ ./android.nix ./go.nix ./haskell.nix ./mysql.nix ./rust.nix ];
}
