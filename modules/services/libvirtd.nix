{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.libvirtd;
in {
  options.dotfiles.services.libvirtd.enable = lib.mkEnableOption "libvirtd";

  config = mkIf cfg.enable {
    nixpkgs.config.allowUnfree = true;

    virtualisation.libvirtd.enable = true;

    environment.systemPackages = with pkgs; [ virtmanager ];

    users.extraGroups.libvirtd.members = [ dotfiles.user ];
  };
}
