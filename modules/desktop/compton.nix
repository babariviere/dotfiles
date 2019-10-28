{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.compton;
in {
  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    services.compton = {
      enable = true;
      fade = true;
      fadeDelta = 3;
      backend = "glx";
      activeOpacity = "1.0";
      inactiveOpacity = "0.9";
    };
  };
}
