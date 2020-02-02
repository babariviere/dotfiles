{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.social.keybase;
in {
  options.dotfiles.social.keybase.enable = lib.mkEnableOption "keybase";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ keybase-gui ];
  };
}
