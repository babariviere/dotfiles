{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.brave;
in {
  options.dotfiles.desktop.brave.enable = lib.mkEnableOption "brave";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = [ pkgs.unstable.brave ];
  };
}
