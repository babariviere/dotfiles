{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.media.plex;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ plex-media-player ];
  };
}