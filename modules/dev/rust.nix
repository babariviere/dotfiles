{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  config = lib.mkIf cfg.dev.rust.enable {
    nixpkgs.overlays = [
      (import (builtins.fetchTarball
        "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz"))
    ];

    environment.systemPackages = with pkgs;
      [ latest.rustChannels.nightly.rust ];
  };
}
