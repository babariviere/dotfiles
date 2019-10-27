{ config, lib, pkgs, ... }:

let
  unstable = import <nixpkgs-unstable> { };
  cfg = config.services.dotfiles.tools.devops;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with unstable; [ terraform packer ansible ];
  };
}
