{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.thunderbird;
in {
  options.dotfiles.desktop.thunderbird.enable =
    lib.mkEnableOption "thunderbird";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ thunderbird ];
  };
}
