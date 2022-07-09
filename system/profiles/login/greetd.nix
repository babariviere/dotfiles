{ config, lib, pkgs }:

let cfg = config.profiles.login.greetd;
in {
  options = {
    command = lib.mkOption {
      type = lib.types.str;
      example = "sway";
      description = "command to be run at startup";
    };
  };
  config = {
    services.greetd = {
      enable = true;
      vt = 7;
      settings = {
        default_session.command =
          ''${pkgs.greetd.tuigreet}/bin/tuigreet --cmd "${cfg.command}"'';
      };
    };
    systemd.services.greetd.serviceConfig.Type = "idle";

    # FIXME: what do I need to do this?
    users.users.greeter.group = "greeter";
    users.groups.greeter = { };
  };
}
