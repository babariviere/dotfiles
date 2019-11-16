{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.media.spotify;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ spotify ];
  };
}
