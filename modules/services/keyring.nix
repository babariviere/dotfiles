{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.keyring;
in {
  options.dotfiles.services.keyring.enable = lib.mkEnableOption "keyring";

  config = mkIf cfg.enable {
    programs.seahorse.enable = true;
    security.pam.services.lightdm.enableGnomeKeyring = true;

    environment.systemPackages = with pkgs; [ libsecret ];

    programs.ssh.askPassword =
      mkIf dotfiles.services.ssh.enable "${pkgs.gnome3.seahorse}/bin/seahorse";

    services.dbus.packages = [ pkgs.gcr ];

    home-manager.users."${dotfiles.user}" = {
      services.gnome-keyring.enable = true;

      services.gpg-agent.pinentryFlavor = "gnome3";

      programs.git = mkIf dotfiles.shell.git.enable {
        extraConfig = {
          credential.helper =
            "${pkgs.gitAndTools.gitFull}/bin/git-credential-libsecret";
        };
      };
    };
  };
}
