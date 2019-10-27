{ config, lib, pkgs, ... }:

let
  dotfiles = config.services.dotfiles;
  cfg = dotfiles.desktop.firefox;
in {
  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ firefox ];
  };
}
