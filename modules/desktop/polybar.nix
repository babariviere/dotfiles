{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.polybar;
in {
  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    home-manager.users."${dotfiles.user}".services.polybar = {
      enable = true;
      package = pkgs.polybar.override {
        i3GapsSupport = true; # TODO: find a way to variabilise it
        alsaSupport = true;
      };
      extraConfig =
        builtins.readFile <config/polybar/config>; # TODO: use config field
      script = "polybar top &";
    };

    fonts.fonts = with pkgs; [ siji ];
  };
}
