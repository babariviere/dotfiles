{ config, lib, pkgs, ... }:

let cfg = config.services.dotfiles.social.discord;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ discord ];
  };
}
