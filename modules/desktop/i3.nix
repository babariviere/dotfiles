{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.i3;
  configFile = pkgs.mutate <config/i3/config> (dotfiles.colors // {
    setWallpaper = "${pkgs.feh}/bin/feh --bg-center ${dotfiles.wallpaper}";
  });
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
        maim # for screenshot
      ];
    };

    # TODO: this causes infinite recursion
    dotfiles.desktop.xinitrc = "exec i3";

    home-manager.users."${dotfiles.user}" = {
      # TODO: find a better way
      home.file = {
        ".xinitrc".text = ''
          #!/bin/sh

          if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
            eval $(dbus-launch --exit-with-session --sh-syntax)
          fi
          systemctl --user import-environment DISPLAY XAUTHORITY

          if command -v dbus-update-activation-environment >/dev/null 2>&1; then
                  dbus-update-activation-environment DISPLAY XAUTHORITY
          fi
          exec i3
        '';
      };

      xdg.configFile."i3/config".source = configFile;
    };
  };
}
