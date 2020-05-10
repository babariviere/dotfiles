{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.gpg;
in {
  options.dotfiles.services.gpg.enable = lib.mkEnableOption "gpg";

  config = mkIf cfg.enable {
    home-manager.users.${dotfiles.user} = {
      services.gpg-agent = {
        enable = true;
        enableExtraSocket = true;
        enableSshSupport = true;
        defaultCacheTtl = 60 * 20; # 20 minutes
        extraConfig = ''
          allow-emacs-pinentry
          allow-loopback-pinentry
        '';
      };
    };
  };
}
