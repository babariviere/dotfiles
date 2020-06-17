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

    boot.extraModprobeConfig = ''
      options kvm_intel nested=1
      options kvm_intel emulate_invalid_guest_state=0
      options kvm ignore_msrs=1
    '';
  };
}
