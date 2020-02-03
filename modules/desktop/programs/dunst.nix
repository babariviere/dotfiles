{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.dunst;
in {
  options.dotfiles.desktop.dunst.enable = lib.mkEnableOption "dunst";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    home-manager.users."${dotfiles.user}".xdg.configFile."dunst/dunstrc".source =
      pkgs.mutate <config/dunst/dunstrc> (dotfiles.colors // {
        rofi = pkgs.rofi;
        browser = pkgs.firefox;
        font = dotfiles.desktop.fonts.sansSerif.name;
      });

    environment.systemPackages = [ pkgs.dunst ];
  };
}
