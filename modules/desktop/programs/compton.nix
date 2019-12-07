{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.compton;
in {
  options.dotfiles.desktop.compton.enable = lib.mkEnableOption "compton";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    services.compton = {
      enable = true;
      fade = true;
      fadeDelta = 3;
      # backend = "glx";
      activeOpacity = "0.98";
      inactiveOpacity = "0.30";
      opacityRules = [ ''95:class_g = "Firefox"'' ];
    };
  };
}
