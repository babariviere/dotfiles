{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles;
  unstable = import <nixpkgs-unstable> { };
in {
  options.dotfiles.dev.go.enable = lib.mkEnableOption "go";

  config = lib.mkIf cfg.dev.go.enable {
    environment.systemPackages = with unstable; [ go gomodifytags gotools ];
  };
}
