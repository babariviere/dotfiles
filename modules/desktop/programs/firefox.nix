{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.firefox;
in {
  options.dotfiles.desktop.firefox.enable = lib.mkEnableOption "firefox";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ firefox ];

    environment.variables = { "BROWSER" = "firefox"; };
  };
}
