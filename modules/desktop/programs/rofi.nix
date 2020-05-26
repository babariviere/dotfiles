{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.rofi;
in {
  options.dotfiles.desktop.rofi.enable = lib.mkEnableOption "rofi";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ rofi ];

    home-manager.users."${dotfiles.user}".xdg.configFile = {
      "rofi/config.rasi".source = pkgs.mutate (usrconf "rofi/config.rasi") {
        theme = dotfiles.theme.name;
        terminal = "${pkgs.termite}/bin/termite";
        font = dotfiles.theme.fonts.mono.name;
      };

      "rofi/${dotfiles.theme.name}.rasi".source =
        pkgs.mutate (usrconf "rofi/theme.rasi") dotfiles.theme.colors;
    };
  };
}
