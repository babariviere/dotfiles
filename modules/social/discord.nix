{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.social.discord;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ discord ];
  };
}
