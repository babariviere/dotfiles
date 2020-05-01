{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.awesome;

  screenshot = pkgs.callPackage ./scripts/screenshot.nix { };

  awesomerc = pkgs.mutate <config/awesome/awesomerc> (dotfiles.colors // {
    setWallpaper = "${pkgs.feh}/bin/feh --bg-fill ${dotfiles.wallpaper}";
  });

  sxhkdrc = pkgs.mutate <config/sxhkd/sxhkdrc> {
    terminal = "${pkgs.termite}/bin/termite";
    lock = "${pkgs.betterlockscreen}/bin/betterlockscreen -l";
    screenshot = "${screenshot}/bin/screenshot";
    polybar = "${pkgs.polybar}/bin/polybar";
    light = "${pkgs.light}/bin/light";
    pamixer = "${pkgs.pamixer}/bin/pamixer";
  };
in {
  options.dotfiles.desktop.awesome.enable = lib.mkEnableOption "awesome";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    services.xserver.windowManager.awesome = { enable = true; };

    home-manager.users."${dotfiles.user}" = {
      # xdg.configFile."awesome/awesomerc".source = awesomerc;
      # xdg.configFile."sxhkd/sxhkdrc".source = sxhkdrc;
    };
  };
}
