{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.tools.light;
in {
  config = lib.mkIf cfg.enable {
    programs.light.enable = true;
    services.actkbd = {
      enable = true;
      bindings = [
        {
          keys = [ 224 ];
          events = [ "key" ];
          command = "/run/current-system/sw/bin/light -U 10";
        }
        {
          keys = [ 225 ];
          events = [ "key" ];
          command = "/run/current-system/sw/bin/light -A 10";
        }
      ];
    };

    # Add user to video group
    users.users."${dotfiles.user}" = { extraGroups = [ "video" ]; };
  };
}
