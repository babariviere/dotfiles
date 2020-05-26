{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.awesome;

  screenshot = pkgs.callPackage ./scripts/screenshot.nix { };

  awesomerc = pkgs.mutate (usrconf "awesome/awesomerc") (dotfiles.theme.colors
    // {
      setWallpaper =
        "${pkgs.feh}/bin/feh --bg-fill ${dotfiles.theme.wallpaper}";
    });

  sxhkdrc = pkgs.mutate (usrconf "sxhkd/sxhkdrc") {
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
    services.xserver.windowManager.awesome = {
      enable = true;
      package = pkgs.awesome.overrideAttrs (old: {
        version = "master";
        src = pkgs.fetchgit {
          url = "https://github.com/awesomeWM/awesome.git";
          rev = "29f6387defd18c1d44b7d2976b48847fe80686b3";
          sha256 = "01chrpp3pzd6ar31zli30hmgyl5h5igw0lkrq0dbv26sqxivd07k";
        };
      });
    };

    services.acpid.enable = true;
    services.upower.enable = true;

    environment.systemPackages = with pkgs; [ material-design-icons ];

    home-manager.users."${dotfiles.user}" = {
      # xdg.configFile."awesome/awesomerc".source = awesomerc;
      # xdg.configFile."sxhkd/sxhkdrc".source = sxhkdrc;
    };
  };
}
