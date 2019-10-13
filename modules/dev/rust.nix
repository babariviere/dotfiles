{ config, lib, pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz))
  ];

  environment.systemPackages = with pkgs; [
    latest.rustChannels.nightly.rust
  ];
}
