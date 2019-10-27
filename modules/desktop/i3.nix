{ config, lib, pkgs, ... }:

let
  dotfiles = config.services.dotfiles;
  cfg = dotfiles.desktop.i3;
in {
  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    services.xserver.windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      configFile = <config/i3/config>;
      extraPackages = with pkgs; [
        i3lock-color
        betterlockscreen
        dmenu
        rofi
        feh
      ];
    };

    home-manager.users."${dotfiles.user}".home = {
      # TODO: find a better way
      file = {
        ".xinitrc".text = ''
          #!/bin/sh

          exec i3 -c ${<config/i3/config>}
        '';
      };

      # TODO: inject config
    };
  };
}
