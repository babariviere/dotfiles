{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.rofi;
in {
  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ rofi ];

    home-manager.users."${dotfiles.user}".xdg.configFile = {
      "rofi/config.rasi".source = pkgs.mutate <config/rofi/config.rasi> {
        theme = dotfiles.theme;
        terminal = "${pkgs.termite}/bin/termite";
      };

      "rofi/${dotfiles.theme}.rasi".source =
        pkgs.mutate <config/rofi/theme.rasi> dotfiles.colors;
    };
  };
}
