{ config, lib, pkgs, ... }:

let
  gcfg = config.services.dotfiles;
  cfg = config.services.desktop;
in
{
  environment.systemPackages = with pkgs; [
    polybar
  ];

  home-manager.users."${gcfg.user}".services.polybar = {
    enable = true;
    package = {
      i3GapsSupport = cfg.wm == "i3";
    };
  };
}
