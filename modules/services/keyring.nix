{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.keyring;
in {
  config = mkIf cfg.enable {
    services.gnome3.gnome-keyring.enable = true;

    environment.systemPackages = with pkgs; [ gnome3.seahorse libsecret ];

    programs.ssh = {
      askPassword = mkIf dotfiles.services.ssh.enable
        "${pkgs.gnome3.seahorse}/libexec/seahorse/ssh-askpass";
    };

    home-manager.users."${dotfiles.user}" = {
      home.file.".gnupg/gpg-agent.conf" = mkIf dotfiles.services.gpg.enable {
        text = "pinentry-program ${pkgs.pinentry_gnome}/bin/pinentry-gnome3";
      };

      programs.git = mkIf dotfiles.shell.git.enable {
        extraConfig = {
          credential.helper =
            "${pkgs.gitAndTools.gitFull}/bin/git-credential-libsecret";
        };
      };
    };
  };
}
