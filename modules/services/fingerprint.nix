{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.fingerprint;
in {
  options.dotfiles.services.fingerprint.enable =
    lib.mkEnableOption "fingerprint";

  config = mkIf cfg.enable {
    services.fprintd = {
      enable = true;
      package = pkgs.fprintd_1_90;
    };

    security.pam.services.login.fprintAuth = true;
    security.pam.services.lightdm.fprintAuth = true;
    security.pam.services.sudo.fprintAuth = true;
    security.pam.services.su.fprintAuth = true;
  };
}
