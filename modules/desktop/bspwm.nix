{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.bspwm;

  screenshot = pkgs.callPackage ./scripts/screenshot.nix { };

  bspwmrc = pkgs.mutate <config/bspwm/bspwmrc> (dotfiles.colors // {
    setWallpaper = "${pkgs.feh}/bin/feh --bg-center ${dotfiles.wallpaper}";
    launchPolybar = "polybar top &"; # TODO: make it optional
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
  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    services.xserver.windowManager.bspwm = { enable = true; };

    dotfiles.desktop.xinitCmd = lib.mkForce "exec bspwm";

    home-manager.users."${dotfiles.user}" = {
      xdg.configFile."bspwm/bspwmrc".source = bspwmrc;
      xdg.configFile."sxhkd/sxhkdrc".source = sxhkdrc;
    };
  };
}
