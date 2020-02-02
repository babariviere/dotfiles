{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.media.plex;
in {
  options.dotfiles.media.plex.enable = lib.mkEnableOption "plex";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ plex-media-player ];
  };
}
