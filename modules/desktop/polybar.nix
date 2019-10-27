{ config, lib, pkgs, ... }:

let
  gcfg = config.services.dotfiles;
  cfg = gcfg.desktop.polybar;
in {
  config = lib.mkIf cfg.enable {
    home-manager.users."${gcfg.user}".services.polybar = {
      enable = true;
      package = pkgs.polybar.override {
        i3GapsSupport = true; # TODO: find a way to variabilise it
        alsaSupport = true;
      };
      extraConfig =
        builtins.readFile <config/polybar/config>; # TODO: use config field
      script = "polybar top &";
    };
  };
}
