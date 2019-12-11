{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.services.mail;
  lookup = email: domain:
    "${pkgs.libsecret}/bin/secret-tool lookup email ${email} domain ${domain}";
  notmuchrc =
    config.home-manager.users."${dotfiles.user}".home.sessionVariables.NOTMUCH_CONFIG;
in {
  config = mkIf cfg.enable {
    environment.variables = {
      NOTMUCH_CONFIG =
        "${config.users.users.${dotfiles.user}.home}/.config/notmuch/notmuchrc";
    };

    home-manager.users."${dotfiles.user}" = {
      accounts.email = {
        maildirBasePath = "${config.users.users.${dotfiles.user}.home}/.mail";
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
              patterns = [ "*" "[Gmail]*" ];
            };
            realName = dotfiles.name;
            msmtp.enable = true;
            notmuch.enable = true;
          };
          website = {
            address = "contact@babariviere.com";
            userName = "contact@babariviere.com";
            aliases = [ "job@babariviere.com" "dev@babariviere.com" ];
            flavor = "plain";
            passwordCommand =
              lookup "contact@babariviere.com" "babariviere.com";
            mbsync = {
              enable = true;
              create = "both";
              expunge = "both";
              patterns = [ "*" ];
            };
            imap = {
              host = "mail.babariviere.com";
              port = 143;
              tls = {
                #enable = true;
                useStartTls = true;
              };
            };
            smtp = {
              host = "mail.babariviere.com";
              port = 587;
              tls = {
                #enable = true;
                useStartTls = true;
              };
            };
            realName = dotfiles.name;
            msmtp.enable = true;
            notmuch.enable = true;
          };
        };
      };

      programs = {
        msmtp.enable = true;
        mbsync.enable = true;
        notmuch = {
          enable = true;
          # TODO: setup hooks with afew or else
          new = {
            ignore = [ "trash" "*.json" ];
            tags = [ "new" ];
          };
          maildir.synchronizeFlags = true;
        };
      };

      services.mbsync = {
        enable = true;
        frequency = "*:0/15";
        # TODO: setup preExec with afew
        postExec = "${pkgs.notmuch}/bin/notmuch --config=${notmuchrc} new";
      };
    };
  };
}
