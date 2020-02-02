{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.fwupd;
in {
  options.dotfiles.services.fwupd.enable = lib.mkEnableOption "fwupd";

  config = mkIf cfg.enable { services.fwupd.enable = true; };
}
