{ config, lib, pkgs, ... }:

let
  unstable = import <nixpkgs-unstable> { };
  cfg = config.dotfiles.tools.devops;
in {
  options.dotfiles.tools.devops.enable = lib.mkEnableOption "devops";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with unstable; [
      terraform
      packer
      ansible
      nixops
    ];
  };
}
