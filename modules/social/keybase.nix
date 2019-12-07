{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.social.keybase;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ keybase-gui ];
  };
}
