{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.chrome;
in {
  options.dotfiles.desktop.chrome.enable = lib.mkEnableOption "chrome";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ google-chrome ];
  };
}
