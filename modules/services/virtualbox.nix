{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.virtualbox;
in {
  options.dotfiles.services.virtualbox.enable = lib.mkEnableOption "virtualbox";

  config = mkIf cfg.enable {
    nixpkgs.config.allowUnfree = true;

    virtualisation.virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };

    users.extraGroups.vboxusers.members = [ dotfiles.user ];
  };
}
