{ config, lib, pkgs, ... }:

let
  gcfg = config.services.dotfiles;
  cfg = config.services.desktop;
in
{
  home-manager.users."${gcfg.user}".services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      i3GapsSupport = true; # TODO: find a way to variabilise it
      alsaSupport = true;
    };
    extraConfig = builtins.readFile <config/polybar/config>; # TODO: use config field
    script = "polybar main &"; 
  };
}
