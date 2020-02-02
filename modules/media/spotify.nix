{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.media.spotify;
in {
  options.dotfiles.media.spotify.enable = lib.mkEnableOption "spotify";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ spotify ];
  };
}
