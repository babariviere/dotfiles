{ config, lib, pkgs, usrconf, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.i3;

  screenshot = pkgs.callPackage ./scripts/screenshot.nix { };

  configFile = pkgs.mutate (usrconf "i3/config") (dotfiles.theme.colors // {
    setWallpaper = "${pkgs.feh}/bin/feh --bg-fill ${dotfiles.theme.wallpaper}";
    screenshot = "${screenshot}/bin/screenshot";
  });
in {
  options.dotfiles.desktop.i3.enable = mkEnableOption "i3";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    services.xserver.windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      extraPackages = with pkgs; [
        i3lock-color
        betterlockscreen
        rofi
        feh
        maim # for screenshot
        screenshot
      ];
    };

    home-manager.users."${dotfiles.user}" = {
      xdg.configFile."i3/config".source = configFile;
    };
  };
}
