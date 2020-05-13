{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.mail;
  lookup = email: domain:
    "${pkgs.gnupg}/bin/gpg2 -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | ${pkgs.gawk}/bin/awk '/machine imap.${domain} login ${email}/{print $NF}'";
  maildir = "${config.users.users.${dotfiles.user}.home}/.mail";
in {
  options.dotfiles.services.mail.enable = lib.mkEnableOption "mail";

  config = mkIf cfg.enable {
    environment.systemPackages = singleton pkgs.unstable.mu;

    home-manager.users."${dotfiles.user}" = {
      accounts.email = {
        maildirBasePath = maildir;
        accounts = {
          gmail = {
            address = "babathriviere@gmail.com";
            userName = "babathriviere@gmail.com";
            flavor = "gmail.com";
            primary = true;
            passwordCommand = lookup "babathriviere@gmail.com" "gmail.com";
            mbsync = {
              enable = true;
              create = "both";
              expunge = "both";
              patterns = [
                "Inbox"
                "[Gmail]/Sent Mail"
                "[Gmail]/Drafts"
                "[Gmail]/Trash"
                "[Gmail]/All Mail"
              ];
            };
            realName = dotfiles.name;
            msmtp.enable = true;
          };
        };
      };

      programs = {
        msmtp.enable = true;
        mbsync.enable = true;
      };

      # services.mbsync = {
      #   enable = true;
      #   frequency = "*:0/15";

      #   preExec = "${pkgs.isync}/bin/mbsync -Ha";
      #   # Emacs starts a server, this is useless
      #   # postExec = "${pkgs.unstable.mu}/bin/mu index";
      # };
    };
  };
}
